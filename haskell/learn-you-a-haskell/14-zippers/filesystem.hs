import Data.List (break)

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
  Folder "root"
      [ File "goat_yelling_like_man.wmv" "baaaaaa"
      , File "pope_time.avi" "god bless"
      , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
      , File "dijon_poupon.doc" "best mustard"
      , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                  [ File "best_hs_prog.hs" "main = print (fix error)"
                  , File "random.hs" "main = print 4"
                  ]
            ]
      ]

data FSCrumb  = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs),bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo file (Folder dir files,bs) = let (ls,x:rs) = break (isFileName file) files
                                  in  (x,FSCrumb dir ls rs:bs)
  where isFileName file (File name _)   = name == file
        isFileName file (Folder name _) = name == file

fsTo' :: Name -> FSZipper -> FSZipper
fsTo' name (Folder folderName items,bs) =
  let (ls,item:rs) = break (nameIs name) items
  in  (item,FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _)     = name == fileName

x -: f = f x

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (File _ content,bs) = (File newName content,bs)
fsRename newName (Folder _ items,bs) = (Folder newName items,bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder name items,bs) = (Folder name (item:items),bs)


-- with failure context:

fsUp3 :: FSZipper -> Maybe FSZipper
fsUp3 (item,FSCrumb name ls rs:bs) = Just (Folder name (ls ++ [item] ++ rs),bs)
fsUp3 (_,[])                       = Nothing

findItem :: Name -> [FSItem] -> Maybe ([FSItem],FSItem,[FSItem])
findItem name items = let (fs,rest) = break (nameIs name) items
                      in case rest of (item:ls) -> Just (fs,item,ls)
                                      otherwise -> Nothing

fsTo3 :: Name -> FSZipper -> Maybe FSZipper
fsTo3 name (Folder folderName items,bs) =
  findItem name items >>= (\(ls,item,rs) -> Just (item,FSCrumb folderName ls rs:bs))
fsTo3 _ (File _ _,_) = Nothing

fsNewFile3 :: FSItem -> FSZipper -> Maybe FSZipper
fsNewFile3 item (Folder name items,bs) = Just (Folder name (item:items),bs)
fsNewFile3 _ (File _ _,_)              = Nothing
