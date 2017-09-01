module UI.Hawt.Component.BorderLayout
(
    createBorderLayout
)
where

    import UI.Hawt.Component
    import UI.Hawt.Render

    data BorderLayout = BorderLayout {  top :: Component,
                                        bottom :: Component,
                                        left :: Component,
                                        right :: Component,
                                        center :: Component }

    createBorderLayout :: Component -> Component -> Component -> Component -> Component -> IO Component
    createBorderLayout t b l r c = createComponent $ BorderLayout t b l r c

    instance IsComponent BorderLayout where
        draw (BorderLayout t b l r c) (width, height) = do
            prefSizeT <- getPrefSize t
            prefSizeB <- getPrefSize b
            prefSizeL <- getPrefSize l
            prefSizeR <- getPrefSize r
            prefSizeC <- getPrefSize c
            let sumcrow = leftWidth + fst prefSizeC + rightWidth
                layoutWidth = maximum [sumcrow, width, fst prefSizeT, fst prefSizeB ]
                maxcrow = maximum [ snd prefSizeL, snd prefSizeC, snd prefSizeR ]
                layoutHeight = maximum [height, maxcrow + topHeight + bottomHeight]
                crowHeight = layoutHeight - topHeight - bottomHeight
                topHeight = snd prefSizeT
                bottomHeight = snd prefSizeB
                centerWidth = layoutWidth - leftWidth - rightWidth
                leftWidth = fst prefSizeL
                rightWidth = fst prefSizeR            
            draw t (layoutWidth, topHeight)
            doWithMovedOffset 0 topHeight $ draw l (leftWidth, crowHeight)
            doWithMovedOffset leftWidth topHeight $ draw c (centerWidth, crowHeight)
            doWithMovedOffset (leftWidth + centerWidth) topHeight $ draw r (rightWidth, crowHeight)
            doWithMovedOffset 0 (topHeight + crowHeight) $ draw b (layoutWidth, bottomHeight)
        getPrefSize (BorderLayout t b l r c) = do
            prefSizeT <- getPrefSize t
            prefSizeB <- getPrefSize b
            prefSizeL <- getPrefSize l
            prefSizeR <- getPrefSize r
            prefSizeC <- getPrefSize c
            let sumcrow = fst prefSizeL + fst prefSizeC + fst prefSizeR
                width = maximum [ sumcrow, fst prefSizeT, fst prefSizeB ]
                maxcrow = maximum [ snd prefSizeL, snd prefSizeC, snd prefSizeR]
                height = maxcrow + snd prefSizeT + snd prefSizeB
            return (width, height)
        handleEvent layout event = return layout
