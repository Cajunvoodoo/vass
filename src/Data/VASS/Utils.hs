{-| Utility functions for working with VASS.

    These are functions which are not directly related to VASS, 
    but provide functionality which is used in a number of different algorithms.

    These operations favour readability over speed.

    If you are implementing an algorithm on VASS and have some functionality
    which you think could be shared between implementations, please fork and
    submit a pull request on GitHub!
-}
module Data.VASS.Utils where

import Data.VASS.Internals
import Data.VASS

{-| 
    Given a VASS, turn all its transitions round and switch the deltas. 

    We get a nice law from this:
    
    prop> reachable initial target vass <=> reachable target initial (reverse vass)

    But we __do not__ get

    prop> coverable initial target vass <=> coverable target initial (reverse vass)

    (@target = \<0, 0\>@ induces some obvious counterexamples.)
-}
reverse :: VASS -> VASS
reverse v@VASS{..} = let
    
    rearrange :: (Name State, Transition) -> (Name State, Transition)
    rearrange (s, Transition name pre post s') 
        = (s', Transition name post pre s)

    in v 
        { transitions = collate . map rearrange . decollate $ transitions }