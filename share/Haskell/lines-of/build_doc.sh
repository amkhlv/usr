#!/bin/bash

stack exec -- haddock src/*.hs tables/*.hs -o docs -h
