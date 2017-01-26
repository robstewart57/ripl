module XDFNetwork where

import Types

type XML = String

xmlFromProgramConnections :: CalProject -> [String] -> Int -> Int -> XML
-- xml actors conns = unlines $
xmlFromProgramConnections (CalProject actors connections) unusedActors fifoDepth outBitWidth =
  unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<XDF name=\"ProgramNetwork\">"
  , unlines (ioPorts outBitWidth)
  , unlines (instances actors)
  , unlines (xdfNetwork fifoDepth connections)
  , unlines (connectUnusedActors unusedActors)
  , "</XDF>"
  ]

xmlFromTopLevelIOConnections :: XML
xmlFromTopLevelIOConnections =
  unlines $
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<XDF name=\"TopNetwork\">"
  , unlines (instances ioActors)
  , unlines (xdfNetworkIO ioConnections)
  , "</XDF>"
  ]

connectUnusedActors actors =
  concatMap (\(actor, n) -> dummyN actor n) (zip actors [1 ..])
  where
    dummyN actorName i =
      [ "<Port kind=\"Output\" name=\"dummy" ++ show i ++ "\">"
      , "  <Type name=\"int\">"
      , "    <Entry kind=\"Expr\" name=\"size\">"
      , "          <Expr kind=\"Literal\" literal-kind=\"Integer\" value=\"16\"/>"
      , "    </Entry>"
      , " </Type>"
      , "</Port>"
      , "<Connection dst=\"\" dst-port=\"dummy" ++
        show i ++ "\" src=\"" ++ actorName ++ "\" src-port=\"Out1\"/>"
      ]

ioPorts outBitWidth =
  [ "<Port kind=\"Input\" name=\"In\">"
  , "   <Type name=\"int\">"
  , "       <Entry kind=\"Expr\" name=\"size\">"
  , "           <Expr kind=\"Literal\" literal-kind=\"Integer\" value=\"16\"/>"
  , "       </Entry>"
  , "   </Type>"
  , "</Port>"
  , ""
  , "<Port kind=\"Output\" name=\"Out\">"
  , "   <Type name=\"int\">"
  , "       <Entry kind=\"Expr\" name=\"size\">"
  , "           <Expr kind=\"Literal\" literal-kind=\"Integer\" value=\"" ++
--    show outBitWidth ++ "\"/>"
      "16" ++ "\"/>"
  , "       </Entry>"
  , "   </Type>"
  , "</Port>"
  ]

ioActors =
  [ IncludeActor "std.stdio" "FileReader"
  , IncludeActor "std.stdio" "StreamToGrey"
  , IncludeActor "std.stdio" "EndOfStream"
  -- , IncludeActor "std.stdio" "castU8ToI16"
  , IncludeActor "std.stdio" "YUVToStream"
  , IncludeActor "std.stdio" "Writer"
  , IncludeActor "xdf" "ProgNetwork"
  ]

ioConnections =
  [ Connection
    {src = Actor "FileReader" "O", dest = Actor "StreamToGrey" "stream"}
  , Connection {src = Actor "EndOfStream" "Out", dest = Actor "Writer" "Byte"}
  , Connection {src = Actor "EndOfStream" "pEOF", dest = Actor "Writer" "pEOF"}
  , Connection {src = Actor "StreamToGrey" "G", dest = Node "ProgNetwork" "In"}
  -- , Connection {src = Actor "StreamToGrey" "G", dest = Node "castU8ToI16" "Byte"}
  -- , Connection {src = Actor "castU8ToI16" "Out", dest = Node "ProgNetwork" "In"}
  , Connection {src = Node "ProgNetwork" "Out", dest = Actor "YUVToStream" "Y"}
  , Connection
    {src = Actor "YUVToStream" "YUV", dest = Actor "EndOfStream" "In"}
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
  show srcPortType ++ "\">" ++ depthAttr fifoDepth ++ "</Connection>"
xdfConnection fifoDepth (Connection (Actor srcName srcPort) (Port destPortType)) =
  "<Connection dst=\"\" dst-port=\"" ++
  show destPortType ++
  "\" src=\"" ++
  srcName ++
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
