data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem a)  = [a]
myFlatten (List ns) = concatMap myFlatten ns
