import RecursiveContents (getRecursiveContents)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]

simpleFind selector basePath = getRecursiveContents basePath >>= return . filter selector
