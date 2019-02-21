# Typogenetics
How to build and run this code:
1. Install the haskell build tool 'stack' https://www.haskellstack.org/
   On linux, simply enter the command 'wget -qO- https://get.haskellstack.org/ | sh' (as super user)
2. From this directory, enter the command 'stack build'
   stack will automatically install the compiler and any dependencies into ~/.stack
3. From app/, enter the command 'stack exec ghc -- -O2 Main.hs'
4. From app/, enter the command './Main'
