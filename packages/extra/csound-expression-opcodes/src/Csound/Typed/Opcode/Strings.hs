module Csound.Typed.Opcode.Strings (
    
    
    -- * Definition.
    strfromurl, strget, strset,
    
    -- * Manipulation.
    puts, sprintf, sprintfk, strcat, strcatk, strcmp, strcmpk, strcpy, strcpyk, strindex, strindexk, strlen, strlenk, strrindex, strrindexk, strsub, strsubk,
    
    -- * Conversion.
    strchar, strchark, strlower, strlowerk, strtod, strtodk, strtol, strtolk, strupper, strupperk) where

import Control.Monad.Trans.Class
import Control.Monad
import Csound.Dynamic
import Csound.Typed

-- Definition.

-- | 
-- Set string variable to value read from an URL
--
-- strfromurl sets a string variable at
--       initialization time to the value found from reading an URL.
--
-- > Sdst  strfromurl  StringURL
--
-- csound doc: <http://csound.com/docs/manual/strfromurl.html>
strfromurl ::  Str -> Str
strfromurl b1 =
  Str $ f <$> unStr b1
  where
    f a1 = opcs "strfromurl" [(Sr,[Sr])] [a1]

-- | 
-- Set string variable to value from strset table or string p-field
--
-- strget sets a string variable at initialization time to the value stored in strset table at the specified index, or a string p-field from the score. If there is no string defined for the index, the variable is set to an empty string.
--
-- > Sdst  strget  indx
--
-- csound doc: <http://csound.com/docs/manual/strget.html>
strget ::  D -> Str
strget b1 =
  Str $ f <$> unD b1
  where
    f a1 = opcs "strget" [(Sr,[Ir])] [a1]

-- | 
-- Allows a string to be linked with a numeric value.
--
-- >  strset  iarg, istring
--
-- csound doc: <http://csound.com/docs/manual/strset.html>
strset ::  D -> D -> SE ()
strset b1 b2 =
  SE $ join $ f <$> (lift . unD) b1 <*> (lift . unD) b2
  where
    f a1 a2 = opcsDep_ "strset" [(Xr,[Ir,Ir])] [a1,a2]

-- Manipulation.

-- | 
-- Print a string constant or variable
--
-- puts prints a string with an optional newline at the end whenever the trigger signal is positive and changes.
--
-- >  puts  Sstr, ktrig[, inonl]
--
-- csound doc: <http://csound.com/docs/manual/puts.html>
puts ::  Str -> Sig -> SE ()
puts b1 b2 =
  SE $ join $ f <$> (lift . unStr) b1 <*> (lift . unSig) b2
  where
    f a1 a2 = opcsDep_ "puts" [(Xr,[Sr,Kr,Ir])] [a1,a2]

-- | 
-- printf-style formatted output to a string variable.
--
-- sprintf write printf-style formatted output to a string variable, similarly to the C function sprintf(). sprintf runs at i-time only.
--
-- > Sdst  sprintf  Sfmt, xarg1[, xarg2[, ... ]]
--
-- csound doc: <http://csound.com/docs/manual/sprintf.html>
sprintf ::  Str -> Sig -> Str
sprintf b1 b2 =
  Str $ f <$> unStr b1 <*> unSig b2
  where
    f a1 a2 = opcs "sprintf" [(Sr,[Sr] ++ (repeat Xr))] [a1,a2]

-- | 
-- printf-style formatted output to a string variable at k-rate.
--
-- sprintfk writes printf-style formatted output to a string variable, similarly to the C function sprintf(). sprintfk runs both at initialization and performance time.
--
-- > Sdst  sprintfk  Sfmt, xarg1[, xarg2[, ... ]]
--
-- csound doc: <http://csound.com/docs/manual/sprintfk.html>
sprintfk ::  Str -> Sig -> Str
sprintfk b1 b2 =
  Str $ f <$> unStr b1 <*> unSig b2
  where
    f a1 a2 = opcs "sprintfk" [(Sr,[Sr] ++ (repeat Xr))] [a1,a2]

-- | 
-- Concatenate strings
--
-- Concatenate two strings and store the result in a variable. strcat runs at i-time only. It is allowed for any of the input arguments to be the same as the output variable.
--
-- > Sdst  strcat  Ssrc1, Ssrc2
--
-- csound doc: <http://csound.com/docs/manual/strcat.html>
strcat ::  Str -> Str -> Str
strcat b1 b2 =
  Str $ f <$> unStr b1 <*> unStr b2
  where
    f a1 a2 = opcs "strcat" [(Sr,[Sr,Sr])] [a1,a2]

-- | 
-- Concatenate strings (k-rate)
--
-- Concatenate two strings and store the result in a variable. strcatk does the concatenation both at initialization and performance time. It is allowed for any of the input arguments to be the same as the output variable.
--
-- > Sdst  strcatk  Ssrc1, Ssrc2
--
-- csound doc: <http://csound.com/docs/manual/strcatk.html>
strcatk ::  Str -> Str -> Str
strcatk b1 b2 =
  Str $ f <$> unStr b1 <*> unStr b2
  where
    f a1 a2 = opcs "strcatk" [(Sr,[Sr,Sr])] [a1,a2]

-- | 
-- Compare strings
--
-- Compare strings and set the result to -1, 0, or 1 if the first string is less than, equal to, or greater than the second, respectively. strcmp compares at i-time only.
--
-- > ires  strcmp  S1, S2
--
-- csound doc: <http://csound.com/docs/manual/strcmp.html>
strcmp ::  Str -> Str -> D
strcmp b1 b2 =
  D $ f <$> unStr b1 <*> unStr b2
  where
    f a1 a2 = opcs "strcmp" [(Ir,[Sr,Sr])] [a1,a2]

-- | 
-- Compare strings
--
-- Compare strings and set the result to -1, 0, or 1 if the first string is less than, equal to, or greater than the second, respectively. strcmpk does the comparison both at initialization and performance time.
--
-- > kres  strcmpk  S1, S2
--
-- csound doc: <http://csound.com/docs/manual/strcmpk.html>
strcmpk ::  Str -> Str -> Sig
strcmpk b1 b2 =
  Sig $ f <$> unStr b1 <*> unStr b2
  where
    f a1 a2 = opcs "strcmpk" [(Kr,[Sr,Sr])] [a1,a2]

-- | 
-- Assign value to a string variable
--
-- Assign to a string variable by copying the source which may be a constant or another string variable. strcpy and = copy the string at i-time only.
--
-- > Sdst  strcpy  Ssrc
--
-- csound doc: <http://csound.com/docs/manual/strcpy.html>
strcpy ::  Str -> Str
strcpy b1 =
  Str $ f <$> unStr b1
  where
    f a1 = opcs "strcpy" [(Sr,[Sr])] [a1]

-- | 
-- Assign value to a string variable (k-rate)
--
-- Assign to a string variable by copying the source which may be a constant or another string variable. strcpyk does the assignment both at initialization and performance time.
--
-- > Sdst  strcpyk  Ssrc
--
-- csound doc: <http://csound.com/docs/manual/strcpyk.html>
strcpyk ::  Str -> Str
strcpyk b1 =
  Str $ f <$> unStr b1
  where
    f a1 = opcs "strcpyk" [(Sr,[Sr])] [a1]

-- | 
-- Return the position of the first occurence of a string in another string
--
-- Return the position of the first occurence of S2 in S1, or -1 if not
--       found. If S2 is empty, 0 is returned. strindex runs at init time only.
--
-- > ipos  strindex  S1, S2
--
-- csound doc: <http://csound.com/docs/manual/strindex.html>
strindex ::  Str -> Str -> D
strindex b1 b2 =
  D $ f <$> unStr b1 <*> unStr b2
  where
    f a1 a2 = opcs "strindex" [(Ir,[Sr,Sr])] [a1,a2]

-- | 
-- Return the position of the first occurence of a string in another string
--
-- Return the position of the first occurence of S2 in S1, or -1 if not
--       found. If S2 is empty, 0 is returned. strindexk runs both at init and
--       performance time.
--
-- > kpos  strindexk  S1, S2
--
-- csound doc: <http://csound.com/docs/manual/strindexk.html>
strindexk ::  Str -> Str -> Sig
strindexk b1 b2 =
  Sig $ f <$> unStr b1 <*> unStr b2
  where
    f a1 a2 = opcs "strindexk" [(Kr,[Sr,Sr])] [a1,a2]

-- | 
-- Return the length of a string
--
-- Return the length of a string, or zero if it is empty. strlen runs at init time only.
--
-- > ilen  strlen  Sstr
--
-- csound doc: <http://csound.com/docs/manual/strlen.html>
strlen ::  Str -> D
strlen b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "strlen" [(Ir,[Sr])] [a1]

-- | 
-- Return the length of a string
--
-- Return the length of a string, or zero if it is empty. strlenk runs both at init and performance time.
--
-- > klen  strlenk  Sstr
--
-- csound doc: <http://csound.com/docs/manual/strlenk.html>
strlenk ::  Str -> Sig
strlenk b1 =
  Sig $ f <$> unStr b1
  where
    f a1 = opcs "strlenk" [(Kr,[Sr])] [a1]

-- | 
-- Return the position of the last occurence of a string in another string
--
-- Return the position of the last occurence of S2 in S1, or -1 if not
--       found. If S2 is empty, the length of S1 is returned. strrindex runs
--       at init time only.
--
-- > ipos  strrindex  S1, S2
--
-- csound doc: <http://csound.com/docs/manual/strrindex.html>
strrindex ::  Str -> Str -> D
strrindex b1 b2 =
  D $ f <$> unStr b1 <*> unStr b2
  where
    f a1 a2 = opcs "strrindex" [(Ir,[Sr,Sr])] [a1,a2]

-- | 
-- Return the position of the last occurence of a string in another string
--
-- Return the position of the last occurence of S2 in S1, or -1 if not
--       found. If S2 is empty, the length of S1 is returned. strrindexk runs
--       both at init and performance time.
--
-- > kpos  strrindexk  S1, S2
--
-- csound doc: <http://csound.com/docs/manual/strrindexk.html>
strrindexk ::  Str -> Str -> Sig
strrindexk b1 b2 =
  Sig $ f <$> unStr b1 <*> unStr b2
  where
    f a1 a2 = opcs "strrindexk" [(Kr,[Sr,Sr])] [a1,a2]

-- | 
-- Extract a substring
--
-- Return a substring of the source string. strsub runs at init time only.
--
-- > Sdst  strsub  Ssrc[, istart[, iend]]
--
-- csound doc: <http://csound.com/docs/manual/strsub.html>
strsub ::  Str -> Str
strsub b1 =
  Str $ f <$> unStr b1
  where
    f a1 = opcs "strsub" [(Sr,[Sr,Ir,Ir])] [a1]

-- | 
-- Extract a substring
--
-- Return a substring of the source string. strsubk runs both at init and
--       performance time.
--
-- > Sdst  strsubk  Ssrc, kstart, kend
--
-- csound doc: <http://csound.com/docs/manual/strsubk.html>
strsubk ::  Str -> Sig -> Sig -> Str
strsubk b1 b2 b3 =
  Str $ f <$> unStr b1 <*> unSig b2 <*> unSig b3
  where
    f a1 a2 a3 = opcs "strsubk" [(Sr,[Sr,Kr,Kr])] [a1,a2,a3]

-- Conversion.

-- | 
-- Return the ASCII code of a character in a string
--
-- Return the ASCII code of the character in Sstr at ipos (defaults to zero
--       which means the first character), or zero if ipos is out of range.
--       strchar runs at init time only.
--
-- > ichr  strchar  Sstr[, ipos]
--
-- csound doc: <http://csound.com/docs/manual/strchar.html>
strchar ::  Str -> D
strchar b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "strchar" [(Ir,[Sr,Ir])] [a1]

-- | 
-- Return the ASCII code of a character in a string
--
-- Return the ASCII code of the character in Sstr at kpos (defaults to zero
--       which means the first character), or zero if kpos is out of range.
--       strchark runs both at init and performance time.
--
-- > kchr  strchark  Sstr[, kpos]
--
-- csound doc: <http://csound.com/docs/manual/strchark.html>
strchark ::  Str -> Sig
strchark b1 =
  Sig $ f <$> unStr b1
  where
    f a1 = opcs "strchark" [(Kr,[Sr,Kr])] [a1]

-- | 
-- Convert a string to lower case
--
-- Convert Ssrc to lower case, and write the result to Sdst.
--       strlower runs at init time only.
--
-- > Sdst  strlower  Ssrc
--
-- csound doc: <http://csound.com/docs/manual/strlower.html>
strlower ::  Str -> Str
strlower b1 =
  Str $ f <$> unStr b1
  where
    f a1 = opcs "strlower" [(Sr,[Sr])] [a1]

-- | 
-- Convert a string to lower case
--
-- Convert Ssrc to lower case, and write the result to Sdst.
--       strlowerk runs both at init and performance time.
--
-- > Sdst  strlowerk  Ssrc
--
-- csound doc: <http://csound.com/docs/manual/strlowerk.html>
strlowerk ::  Str -> Str
strlowerk b1 =
  Str $ f <$> unStr b1
  where
    f a1 = opcs "strlowerk" [(Sr,[Sr])] [a1]

-- | 
-- Converts a string to a float (i-rate).
--
-- Convert a string to a floating point value. It is also possible to
--     pass an strset index or a string p-field from the score instead of a string
--     argument. If the string cannot be parsed as a floating point or integer number, an init or perf error occurs and the instrument is deactivated.
--
-- > ir  strtod  Sstr
-- > ir  strtod  indx
--
-- csound doc: <http://csound.com/docs/manual/strtod.html>
strtod ::  Str -> D
strtod b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "strtod" [(Ir,[Sr]),(Ir,[Ir])] [a1]

-- | 
-- Converts a string to a float (k-rate).
--
-- Convert a string to a floating point value at i- or k-rate. It is also possible to pass an strset index or a string p-field from the score instead of a string
--     argument. If the string cannot be parsed as a floating point or integer number, an init or perf error occurs and the instrument is deactivated.
--
-- > kr  strtodk  Sstr
-- > kr  strtodk  kndx
--
-- csound doc: <http://csound.com/docs/manual/strtodk.html>
strtodk ::  Str -> Sig
strtodk b1 =
  Sig $ f <$> unStr b1
  where
    f a1 = opcs "strtodk" [(Kr,[Sr]),(Kr,[Kr])] [a1]

-- | 
-- Converts a string to a signed integer (i-rate).
--
-- Convert a string to a signed integer value. It is also possible to
--     pass an strset index or a string p-field from the score instead of a string
--     argument. If the string cannot be parsed as an integer number, an init error occurs and the instrument is deactivated.
--
-- > ir  strtol  Sstr
-- > ir  strtol  indx
--
-- csound doc: <http://csound.com/docs/manual/strtol.html>
strtol ::  Str -> D
strtol b1 =
  D $ f <$> unStr b1
  where
    f a1 = opcs "strtol" [(Ir,[Sr]),(Ir,[Ir])] [a1]

-- | 
-- Converts a string to a signed integer (k-rate).
--
-- Convert a string to a signed integer value at i- or k-rate. It is also possible to pass an strset index or a string p-field from the score instead of a string
--     argument. If the string cannot be parsed as an integer number, an init or perf error occurs and the instrument is deactivated.
--
-- > kr  strtolk  Sstr
-- > kr  strtolk  kndx
--
-- csound doc: <http://csound.com/docs/manual/strtolk.html>
strtolk ::  Str -> Sig
strtolk b1 =
  Sig $ f <$> unStr b1
  where
    f a1 = opcs "strtolk" [(Kr,[Sr]),(Kr,[Kr])] [a1]

-- | 
-- Convert a string to upper case
--
-- Convert Ssrc to upper case, and write the result to Sdst.
--       strupper runs at init time only.
--
-- > Sdst  strupper  Ssrc
--
-- csound doc: <http://csound.com/docs/manual/strupper.html>
strupper ::  Str -> Str
strupper b1 =
  Str $ f <$> unStr b1
  where
    f a1 = opcs "strupper" [(Sr,[Sr])] [a1]

-- | 
-- Convert a string to upper case
--
-- Convert Ssrc to upper case, and write the result to Sdst.
--       strupperk runs both at init and performance time.
--
-- > Sdst  strupperk  Ssrc
--
-- csound doc: <http://csound.com/docs/manual/strupperk.html>
strupperk ::  Str -> Str
strupperk b1 =
  Str $ f <$> unStr b1
  where
    f a1 = opcs "strupperk" [(Sr,[Sr])] [a1]