module XDFNetwork where

import Types

type XML = String

xmlFromProgramConnections :: CalProject -> [String] -> Int -> Int -> Int -> XML
-- xml actors conns = unlines $
xmlFromProgramConnections (CalProject actors connections) unusedActors fifoDepth {- outBitWidth -} inputColour outputColour =
  unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<XDF name=\"ProgramNetwork\">"
  , unlines (ioPorts {- outBitWidth -} inputColour outputColour)
  , unlines (instances actors)
  , unlines (xdfNetwork fifoDepth connections)
  -- TODO: add this back in
  -- , unlines (connectUnusedActors unusedActors)
  , "</XDF>"
  ]

xmlFromTopLevelIOConnections :: Int -> Int -> XML
xmlFromTopLevelIOConnections inputColour outputColour =
  unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<XDF name=\"TopNetwork\">"
  , unlines (instances (ioActors inputColour outputColour))
  , unlines (xdfNetworkIO (ioConnections inputColour outputColour))
  , "</XDF>"
  ]

connectUnusedActors actors =
  concatMap (\(actor, n) -> dummyN actor n) (zip actors [1 ..])
  where
    dummyN actorName i =
      [ "<Port kind=\"Output\" name=\"dummy" ++ show i ++ "\">"
      , "  <Type name=\"int\">"
      , "    <Entry kind=\"Expr\" name=\"size\">"
      , "          <Expr kind=\"Literal\" literal-kind=\"Integer\" value=\"32\"/>"
      , "    </Entry>"
      , " </Type>"
      , "</Port>"
      , "<Connection dst=\"\" dst-port=\"dummy" ++
        show i ++ "\" src=\"" ++ actorName ++ "\" src-port=\"Out1\"/>"
      ]

ioPorts {- outBitWidth -} inputColours outputColours =
  concatMap (\i ->
  [ "<Port kind=\"Input\" name=\"In" ++ show i ++ "\">"
  , "   <Type name=\"int\">"
  , "       <Entry kind=\"Expr\" name=\"size\">"
  , "           <Expr kind=\"Literal\" literal-kind=\"Integer\" value=\"32\"/>"
  , "       </Entry>"
  , "   </Type>"
  , "</Port>"
  ])
  [1.. inputColours]
  ++
  concatMap (\i ->
  [ "<Port kind=\"Output\" name=\"Out" ++ show i ++ "\">"
  , "   <Type name=\"int\">"
  , "       <Entry kind=\"Expr\" name=\"size\">"
  , "           <Expr kind=\"Literal\" literal-kind=\"Integer\" value=\"" ++
      "32" ++ "\"/>"
  , "       </Entry>"
  , "   </Type>"
  , "</Port>"
  ])
  [1..outputColours]

ioActors inColourChans outColourChans =
  [ IncludeActor "std.stdio" "FileReader"
  ]
  ++ case inColourChans of
       1 -> [IncludeActor "std.stdio" "StreamToGrey"]
       3 -> [ IncludeActor "std.stdio" "StreamToYUV3Ports"
            , IncludeActor "std.stdio" "YUVToRGB"
            ]
  ++
  [ IncludeActor "std.stdio" "EndOfStream"]
  -- , IncludeActor "std.stdio" "castU8ToI16"
  ++ case outColourChans of
      1 ->
        [IncludeActor "std.stdio" "YToStream"]
      3 ->
        [ IncludeActor "std.stdio" "RGBToYUV"
        , IncludeActor "std.stdio" "YUVToStream"
        ]
  ++
  [ IncludeActor "std.stdio" "Writer"
  , IncludeActor "xdf" "ProgNetwork"
  ]

ioConnections inColourChans outColourChans =
  [ Connection {src = Actor "EndOfStream" "Out", dest = Actor "Writer" "Byte"}
  , Connection {src = Actor "EndOfStream" "pEOF", dest = Actor "Writer" "pEOF"}
  ] ++
  -- , Connection {src = Actor "StreamToGrey" "G", dest = Node "ProgNetwork" "In"}
   case inColourChans of
      1 ->
        [ Connection {src = Actor "StreamToGrey" "G", dest = Node "ProgNetwork" "In1"}
        , Connection {src = Actor "FileReader" "O", dest = Actor "StreamToGrey" "stream"}
        ]
      3 ->
        [ Connection {src = Actor "FileReader" "O", dest = Actor "StreamToYUV3Ports" "stream"}
        , Connection {src = Actor "StreamToYUV3Ports" "Y", dest = Actor "YUVToRGB" "Y"}
        , Connection {src = Actor "StreamToYUV3Ports" "U", dest = Actor "YUVToRGB" "U"}
        , Connection {src = Actor "StreamToYUV3Ports" "V", dest = Actor "YUVToRGB" "V"}
        , Connection {src = Actor "YUVToRGB" "R",    dest = Node "ProgNetwork" "In1"}
        , Connection {src = Actor "YUVToRGB" "G", dest = Node "ProgNetwork" "In2"}
        , Connection {src = Actor "YUVToRGB" "B", dest = Node "ProgNetwork" "In3"}
        ]
  ++
  -- [ Connection {src = Node "ProgNetwork" "Out", dest = Actor "YUVToStream" "Y"}
  -- , Connection
  --   {src = Actor "YUVToStream" "YUV", dest = Actor "EndOfStream" "In"}
  -- ]
   case outColourChans of
     1 ->
       [ Connection {src = Node "ProgNetwork" "Out1", dest = Actor "YToStream" "Y"}
       , Connection {src = Actor "YToStream" "YUV", dest = Actor "EndOfStream" "In"}
       ]
     3 ->
       [ Connection {src = Node "ProgNetwork" "Out1", dest = Actor "RGBToYUV" "R"}
       , Connection {src = Node "ProgNetwork" "Out2", dest = Actor "RGBToYUV" "G"}
       , Connection {src = Node "ProgNetwork" "Out3", dest = Actor "RGBToYUV" "B"}
       , Connection {src = Node "RGBToYUV" "Y", dest = Actor "YUVToStream" "Y"}
       , Connection {src = Node "RGBToYUV" "U", dest = Actor "YUVToStream" "U"}
       , Connection {src = Node "RGBToYUV" "V", dest = Actor "YUVToStream" "V"}
       , Connection {src = Actor "YUVToStream" "YUV", dest = Actor "EndOfStream" "In"}
       ]


instances = map anInstance

anInstance actor =
  case actor of
    RiplUnit {} -> ""
    _ ->
      unlines $
      [ "<Instance id=\"" ++ actorName actor ++ "\">"
      , "  <Class name=\"" ++ package actor ++ "." ++ actorName actor ++ "\"/>"
      , "</Instance>"
      ]

xdfNetwork :: Int -> Connections -> [String]
xdfNetwork fifoDepth = map (xdfConnection fifoDepth)

xdfNetworkIO :: Connections -> [String]
xdfNetworkIO = map (xdfConnection 512)

-- toConnection :: ((String,String),(String,String)) -> String
depthAttr :: Int -> String
depthAttr fifoDepth = ""
  -- unlines
  --   [ " <Attribute kind=\"Value\" name=\"bufferSize\">"
  --   , "   <Expr kind=\"Literal\" literal-kind=\"Integer\" value=\"" ++
  --     show fifoDepth ++ "\"/>"
  --   , " </Attribute>"
  --   ]

xdfConnection :: Int -> Connection -> String
-- xdfConnection (Connection srcActor srcPort destActor destPort) =
xdfConnection fifoDepth (Connection (Actor srcName srcPort) (Actor destName destPort)) =
  "<Connection dst=\"" ++
  destName ++
  "\" dst-port=\"" ++
  destPort ++
  "\" src=\"" ++
  srcName ++
  "\" src-port=\"" ++ srcPort ++ "\">" ++ depthAttr fifoDepth ++ "</Connection>"
xdfConnection fifoDepth (Connection (Port srcPortType) (Actor destName destPort)) =
  "<Connection dst=\"" ++
  destName ++
  "\" dst-port=\"" ++
  destPort ++
  "\" src=\"\" src-port=\"" ++
  srcPortType ++ "\">" ++ depthAttr fifoDepth ++ "</Connection>"
xdfConnection fifoDepth (Connection (Actor srcName srcPort) (Port destPortType)) =
  "<Connection dst=\"\" dst-port=\"" ++
  destPortType ++
  "\" src=\"" ++
  srcName ++
  "\" src-port=\"" ++ srcPort ++ "\">" ++ depthAttr fifoDepth ++ "</Connection>"
-- a simple program, where imread is connected to imwrite
xdfConnection fifoDepth (Connection (Port srcPort) (Port destPortType)) =
  "<Connection dst=\"\" dst-port=\"" ++
  destPortType ++
  "\" src=\"\"" ++
  "\" src-port=\"" ++ srcPort ++ "\">" ++ depthAttr fifoDepth ++ "</Connection>"
xdfConnection fifoDepth (Connection (Node networkName networkPort) (Actor destName destPort)) =
  "<Connection dst=\"" ++
  destName ++
  "\" dst-port=\"" ++
  destPort ++
  "\" src=\"" ++
  networkName ++
  "\" src-port=\"" ++
  networkPort ++ "\">" ++ depthAttr fifoDepth ++ "</Connection>"
xdfConnection fifoDepth (Connection (Actor srcName srcPort) (Node networkName networkPort)) =
  "<Connection dst=\"" ++
  networkName ++
  "\" dst-port=\"" ++
  networkPort ++
  "\" src=\"" ++
  srcName ++
  "\" src-port=\"" ++ srcPort ++ "\">" ++ depthAttr fifoDepth ++ "</Connection>"
