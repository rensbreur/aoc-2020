-- findOuter :: [Rule] -> Color -> Set Color
-- findOuter regulations clr = go (Set.singleton clr) where
--   go clrs = case find (\(bag, inner) -> bag `notMember` clrs && not (clrs `disjoint` inner)) regulations of
--     Nothing -> clrs 
--     Just (newBag, _) -> go (Set.insert newBag clrs)

-- outer :: Regulations -> Color -> Set.Set Color
-- outer regulations color = go Set.empty [color] where
--   go traversed []          = traversed
--   go traversed (next:rest) | next `Set.member` traversed = go traversed rest
--                            | otherwise                   = go (Set.insert next traversed) (containedIn regulations next ++ rest) 


outer2 :: Regulations -> Color -> Set.Set Color
outer2 regulations color = go Set.empty [color] where
  go traversed []          = trace "outer2 complete" traversed
  go traversed (next:rest) = trace ("outer2" ++ next) $ go (Set.insert next traversed) (rest ++ (containedIn regulations next \\ Set.toAscList traversed))


-- implement with State
outer3 :: Regulations -> Color -> State (Set.Set Color) ()
outer3 regs clr = do
  modify $ Set.insert clr
  forM_ (containedIn regs clr) $ \z -> do
    clrs <- get
    when (z `notElem` clrs) $ outer3 regs z