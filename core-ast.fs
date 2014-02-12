let isStraight hand =				/// hand = [(v 8, ``♠``); (Q, ``♦``); (v 9, ``♦``); (v 10, ``♥``); (J, ``♣``)]
	checkHand hand
	let ranks = sortRanks hand		/// ranks = [v 8; v 9; v 10; J; Q]
	let generateStraight start =		/// start = v 8
		rankOrder				///= [v 2; v 3; v 4; v 5; v 6; v 7; v 8; v 9; v 10; J; Q; K; A]
		|> Seq.skipWhile ((<>) start)	///= seq [v 8; v 9; v 10; J; Q; K; A]
		|> Seq.take 5				///= seq [v 8; v 9; v 10; J; Q]
		|> Seq.toList				///= [v 8; v 9; v 10; J; Q]
	ranks = generateStraight (ranks.[0])		///= true
	|| ranks = straightFromOne  // special case Ace = v 1

let isStraight hand =
	checkHand hand
	let ranks = sortRanks hand		/// ranks = [v 8; v 9; v 10; J; Q]
	let generateStraight start =		/// start = v 8
		(|>)
			((|>) 
				((|>)
					rankOrder
					(Seq.skipWhile ((<>) start)))
				(Seq.take 5))
			Seq.toList
	(||)
		((ranks = generateStraight (ranks.[0]))
		(ranks = straightFromOne))

	//let generateStraight start = ((|>) ((|>) ((|>) rankOrder (Seq.skipWhile ((<>) start))) (Seq.take 5)) Seq.toList)
	//let generateStraight start = Seq.toList (Seq.take 5 (Seq.skipWhile ((<>) start) rankOrder)))


Let
         (false,
          [Binding
             (null,NormalBinding,false,false,[],
              PreXmlDoc
                (Microsoft.FSharp.Compiler.Range+pos,
                 Microsoft.FSharp.Compiler.Ast+XmlDocCollector),
              SynValData
                (null,
                 SynValInfo
                   ([[SynArgInfo
                        ([],false,
                         Some
                             {idRange = (77,15--77,19); idText = "hand";})]],SynArgInfo ([],false,null)),
                 null),
              LongIdent
                (LongIdentWithDots
                   ([{idRange = (77,4--77,14); idText = "isStraight";}],[]),null,null,
                 Pats [Named (Wild
                         (77,15--77,19)
                         {idRange = (77,15--77,19); idText = "hand";},false,null,
                       (77,15--77,19)
                 null,
                 (77,4--77,19)
              null,
              Sequential (SequencePointsAtSeq,true,
                 App (NonAtomic,false,
                    Ident {idRange = (78,2--78,11); idText = "checkHand";},
                    Ident {idRange = (78,12--78,16); idText = "hand";},
                    (78,2--78,16)
                 LetOrUse (false,false,
                    [Binding
                       (null,NormalBinding,false,false,[],
                        PreXmlDoc
                          (Microsoft.FSharp.Compiler.Range+pos,
                           Microsoft.FSharp.Compiler.Ast+XmlDocCollector),
                        SynValData (null,SynValInfo ([],SynArgInfo ([],false,null)), null),
                        Named (Wild
                             (79,6--79,11)
                             {idRange = (79,6--79,11); idText = "ranks";},false,null,
                           (79,6--79,11)
                        null,
                        App (NonAtomic,false,
                           Ident {idRange = (79,14--79,23); idText = "sortRanks";},
                           Ident {idRange = (79,24--79,28); idText = "hand";},
                           (79,14--79,28)
                        (79,6--79,11)
                        SequencePointAtBinding
                          (79,2--79,28)
                    LetOrUse
                      (false,false,
                       [Binding
                          (null,NormalBinding,false,false,[],
                           PreXmlDoc
                             (Microsoft.FSharp.Compiler.Range+pos,
                              Microsoft.FSharp.Compiler.Ast+XmlDocCollector),
                           SynValData (null, SynValInfo ([[SynArgInfo ([],false, Some {idRange = (80,23--80,28); idText = "start";})]], SynArgInfo ([],false,null)),null),
                           LongIdent
                             (LongIdentWithDots ([{idRange = (80,6--80,22); idText = "generateStraight";}],[]),null, null,
                              Pats [Named (Wild
                                      (80,23--80,28)
                                      {idRange = (80,23--80,28); idText = "start";},false,null,
                                    (80,23--80,28)
                              null,
                              (80,6--80,28)
                           null,
                           App (NonAtomic,false,
                              App (NonAtomic,true,
                                 Ident {idRange = (84,4--84,6); idText = "op_PipeRight";},
                                 App (NonAtomic,false,
                                    App (NonAtomic,true, Ident {idRange = (83,4--83,6); idText = "op_PipeRight";},
                                       App (NonAtomic,false,
                                          App (NonAtomic,true,
                                             Ident {idRange = (82,4--82,6); idText = "op_PipeRight";},
                                             Ident {idRange = (81,4--81,13); idText = "rankOrder";},
                                             (81,4--82,6)
                                          App (NonAtomic,false,
                                             LongIdent (false,
                                                LongIdentWithDots
                                                  ([{idRange = (82,7--82,10); idText = "Seq";};
                                                      {idRange = (82,11--82,20); idText = "skipWhile";}],
                                                   [(82,10--82,11)
                                                null,
                                                (82,7--82,20)
                                             Paren
                                               (App (NonAtomic,false,
                                                   Ident {idRange = (82,23--82,25); idText = "op_Inequality";},
                                                   Ident {idRange = (82,27--82,32); idText = "start";},
                                                   (82,23--82,32)
                                                (82,21--82,22)
                                                Some
                                                  (82,32--82,33)
                                                (82,21--82,33)
                                             (82,7--82,33)
                                          (81,4--82,33)
                                       (81,4--83,6)
                                    App (NonAtomic,false,
                                       LongIdent (false,
                                          LongIdentWithDots
                                            ([{idRange = (83,7--83,10); idText = "Seq";};
                                                {idRange = (83,11--83,15); idText = "take";}],
                                             [(83,10--83,11)
                                          null,
                                          (83,7--83,15)
                                       Const (Int32 5,
                                          (83,16--83,17)
                                       (83,7--83,17)
                                    (81,4--83,17)
                                 (81,4--84,6)
                              LongIdent
                                (false,
                                 LongIdentWithDots
                                   ([{idRange = (84,7--84,10); idText = "Seq";};
                                       {idRange = (84,11--84,17); idText = "toList";}],
                                    [(84,10--84,11)
                                 null,
                                 (84,7--84,17)
                              (81,4--84,17)
                           (80,6--80,28)
                           SequencePointAtBinding
                             (80,2--84,17)
                       App (NonAtomic,false,
                          App (NonAtomic,true,
                             Ident {idRange = (86,2--86,4); idText = "op_BooleanOr";},
                             App (NonAtomic,false,
                                App (NonAtomic,true,
                                   Ident {idRange = (85,8--85,9); idText = "op_Equality";},
                                   Ident {idRange = (85,2--85,7); idText = "ranks";},
                                   (85,2--85,9)
                                App (NonAtomic,false,
                                   Ident {idRange = (85,10--85,26); idText = "generateStraight";},
                                   Paren (DotIndexedGet
                                        (Ident {idRange = (85,28--85,33); idText = "ranks";},
                                         [One (Const (Int32 0,
                                                (85,35--85,36)
                                         (85,33--85,34)
                                         (85,28--85,37)
                                      (85,27--85,28)
                                      Some
                                        (85,37--85,38)
                                      (85,27--85,38)
                                   (85,10--85,38)
                                (85,2--85,38)
                             (85,2--86,4)
                          App (NonAtomic,false,
                             App  (NonAtomic,true,
                                Ident {idRange = (86,11--86,12); idText = "op_Equality";},
                                Ident {idRange = (86,5--86,10); idText = "ranks";},
                                (86,5--86,12)
                             Ident {idRange = (86,13--86,28); idText = "straightFromOne";},
                             (86,5--86,28)
                          (85,2--86,28)
                       (80,2--86,28)
                    (79,2--86,28)
                 (78,2--86,28)
              (77,4--77,19)
              NoSequencePointAtLetBinding)],
          (77,0--86,28)

