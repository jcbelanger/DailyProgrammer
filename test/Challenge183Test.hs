{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Challenge183Test where
import Challenge183


import Test.Framework
import Data.Text (intercalate)

test_sampleInput = assertEqual output (challenge183 input)
             where input  = intercalate "\n"
                          [ "7"
                          , "2.0.11-alpha"
                          , "0.1.7+amd64"
                          , "0.10.7+20141005"
                          , "2.0.12+i386"
                          , "1.2.34"
                          , "2.0.11+i386"
                          , "20.1.1+i386" ]
                   output = intercalate "\n"
                          [ "0.1.7+amd64"
                          , "0.10.7+20141005"
                          , "1.2.34"
                          , "2.0.11-alpha"
                          , "2.0.11+i386"
                          , "2.0.12+i386"
                          , "20.1.1+i386" ]
