{-|
Module      : Example
Description : Describe sample SQLite table

This module describes a simple SQLite table. 
-}
module Example where

import Common
import GUI



main :: IO()
main = starter $ inject
  "example.css"
  "test.db"
  "test"
  [  ("nick", (10, 1))
  ,  ("name", (20, 3))
  -- (colname, (width, nlines)) 
  ]


