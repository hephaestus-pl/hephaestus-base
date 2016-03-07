# Hephaetus

This is the main repository of Hephaestus, an SPL derivation tool. 

## Installation  

Currently, we are using the Haskell cabal infrastructure to build 
Hephaestus. The following steps should be necessary for a basic 
installation. 

### Clone the repositories

In the target directory, such as $USER_HOME/workspace/hephaestus, you 
must clone several Hephaestus repositories from github. 


> $ git clone https://github.com/hephaestus-pl/funsat.git
>
> $ git clone https://github.com/hephaestus-pl/feature-modelling.git
>
> $ git clone https://github.com/hephaestus-pl/commons.git 
> 
> $ git clone https://github.com/hephaestus-pl/hephaestus-base.git
> 
> $ git clone https://github.com/hephaestus-pl/hephaestus-pp.git


### Create a cabal sandbox

In the target directory, create an hephaestus-sb directory, and initialize 
it as a cabal sandbox. 

> $ mkdir hephaestus-sb
>
> $ cd hephaestus-sb
>
> $ cabal sandbox init --sandbox .
>
> $ cd ..

Initialize all repositories to use that sandbox, and install the respective libraries and tools. 

> $ cd funsat
>
> $ cabal sandbox init --sandbox ../hephaestus-sb
>
> $ cabal install --dependencies-only
>
> $ cabal install
>
> $ cd ..
>
> $ cd commons

> $ cabal sandbox init --sandbox ../hephaestus-sb
>
> $ cabal install --dependencies-only
>
> $ cabal install
>
> $ cd ..

> $ cd feature-modeling 
>
> $ cabal sandbox init --sandbox ../hephaestus-sb
>
> $ cabal install --dependencies-only
>
> $ cabal install

> $ cd ..
>
> $ cd hephaetus-base
>
> $ cabal sandbox init --sandbox ../hephaestus-sb
>
> $ cabal install --dependencies-only
>
> $ cabal install
>
> $ cd ..

This last step should build hephaetus and install it on 
$USER_HOME/workspace/hephaestus/hephaestus-sb/bin. You 
must also copy some scheme files (hephaestus-base/main/src/*.rng) 
to that directory. 

That is all. 

Rodrigo Bonif\'{a}cio - rbonifacio@unb.br