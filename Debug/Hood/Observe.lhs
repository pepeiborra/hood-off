\begin{code}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
\end{code}


%************************************************************************
%*									*
\subsection{Exports}
%*									*
%************************************************************************

\begin{code}
module Debug.Hood.Observe 
  (
   -- * The main Hood API
  

     observe	   -- (Observable a) => String -> a -> a
  , Observer(..)   -- contains a 'forall' typed observe (if supported).
  , Observing      -- a -> a
  , Observable(..) -- Class
  , runO	   -- IO a -> IO ()
  , printO	   -- a -> IO ()
  , putStrO	   -- String -> IO ()

   -- * For advanced users, that want to render their own datatypes.
  , (<<)           -- (Observable a) => ObserverM (a -> b) -> a -> ObserverM b
  , thunk          -- (Observable a) => a -> ObserverM a	
  , send
  , observeBase
  , observeOpaque

  -- * For users that want to write there own render drivers.
  
  , debugO	   -- IO a -> IO [CDS]
  ) where	
\end{code}


%************************************************************************
%*									*
\subsection{Imports and infixing}
%*									*
%************************************************************************

\begin{code}
import System.IO
import Data.Maybe
import Control.Monad
import Data.List
--import System

-- The only non standard one we assume
--import IOExts
import Data.IORef
import System.IO.Unsafe
\end{code}

\begin{code}
import Control.Concurrent
\end{code}

\begin{code}
import Control.Exception ( Exception, throw )
import qualified Control.Exception as Exception
{-
 ( catch
		, Exception(..)
		, throw
		) as Exception
-}
\end{code}

\begin{code}
infixl 9 <<
\end{code}


%************************************************************************
%*									*
\subsection{External start functions}
%*									*
%************************************************************************

Run the observe ridden code.

\begin{code}
printO :: a -> IO ()
printO expr = return ()

putStrO :: String -> IO ()
putStrO expr = return ()

runO :: IO a -> IO ()
runO _ = return ()

debugO _ = return []
\end{code}


%************************************************************************
%*									*
\subsection{Simulations}
%*									*
%************************************************************************

Here we provide stubs for the functionally that is not supported
by some compilers, and provide some combinators of various flavors.



\begin{code}
\end{code}


%************************************************************************
%*									*
\subsection{Instances}
%*									*
%************************************************************************

 The Haskell Base types

\begin{code}
instance Observable a

observeBase :: a -> Parent -> a
observeBase x _ = x

observeOpaque :: String -> a -> Parent -> a
observeOpaque _ val _ = val
\end{code}



%************************************************************************
%*									*
\subsection{Classes and Data Definitions}
%*									*
%************************************************************************

\begin{code}
class Observable a where
	{-
	 - This reveals the name of a specific constructor.
	 - and gets ready to explain the sub-components.
         -
         - We put the context second so we can do eta-reduction
	 - with some of our definitions.
	 -}
	observer  :: a -> Parent -> a 
	{- 
         - This used used to group several observer instances together.
	 -}
	observers :: String -> (Observer -> a) -> a

type Observing a = a -> a
\end{code}

\begin{code}
newtype Observer = O (forall a . (Observable a) => String -> a -> a)
\end{code}


%************************************************************************
%*									*
\subsection{The ObserveM Monad}
%*									*
%************************************************************************

The Observer monad, a simple state monad, 
for placing numbers on sub-observations.

\begin{code}
newtype ObserverM a = ObserverM { runMO :: Int -> Int -> (a,Int) }
\end{code}


%************************************************************************
%*									*
\subsection{observe and friends}
%*									*
%************************************************************************

Our principle function and class

\begin{code}
-- | 'observe' observes data structures in flight.
--  
-- An example of use is 
--  @
--    map (+1) . observe \"intermeduate\" . map (+2)
--  @
--
-- In this example, we observe the value that flows from the producer
-- @map (+2)@ to the consumer @map (+1)@.
-- 
-- 'observe' can also observe functions as well a structural values.
-- 
{-# NOINLINE observe #-}
observe :: (Observable a) => String -> a -> a
observe name a = a

{- This gets called before observer, allowing us to mark
 - we are entering a, before we do case analysis on
 - our object.
 -}

{-# NOINLINE observer_ #-}
observer_ :: (Observable a) => a -> Parent -> a 
observer_ a context = a
\end{code}

\begin{code}
data Parent = Parent
	{ observeParent :: !Int	-- my parent
	, observePort   :: !Int	-- my branch number
	} deriving Show
root = Parent 0 0


instance Monad ObserverM where
	return a = ObserverM (\ c i -> (a,i))
	fn >>= k = ObserverM (\ c i ->
		case runMO fn c i of
		  (r,i2) -> runMO (k r) c i2
		)

thunk :: (Observable a) => a -> ObserverM a
thunk a = ObserverM $ \ parent port ->
		( observer_ a (Parent
				{ observeParent = parent
				, observePort   = port
				}) 
		, port+1 )

(<<) :: (Observable a) => ObserverM (a -> b) -> a -> ObserverM b
fn << a = do { fn' <- fn ; a' <- thunk a ; return (fn' a') }

send = undefined
\end{code}

