default:
	echo "Please, specify sample to compile!"

maybe: TypeClasses.hs Maybe.jhs
	../Preprocessor/joinadsp.exe Maybe.jhs > Maybe.hs
	ghc TypeClasses.hs Maybe.hs

parsers: TypeClasses.hs Parsers.jhs
	../Preprocessor/joinadsp.exe Parsers.jhs > Parsers.hs
	ghc TypeClasses.hs Parsers.hs

parmonad: TypeClasses.hs ParMonad.jhs
	../Preprocessor/joinadsp.exe ParMonad.jhs > ParMonad.hs
	ghc TypeClasses.hs ../ParMonad/Control/Monad/Par.hs ../ParMonad/Control/Monad/Par/Internal.hs ParMonad.hs -threaded -rtsopts

resumptions: TypeClasses.hs Resumptions.jhs
	../Preprocessor/joinadsp.exe Resumptions.jhs > Resumptions.hs
	ghc TypeClasses.hs Resumptions.hs
