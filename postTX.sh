#!/bin/bash

# example of using arguments to a script
echo "Transaction 80a2e90fd2fedee5959c5c9581f37eb0d87b215df889e23ac4bced11a2b75209
  result: "You won!"
  outputs: []
  signers:
    self: 3b344cb8b4e9b06df96054f1c63465504cc46f5a19752d41b2bea47cdbad33cc
    seller: 0331775a097e2b85c1f3cb17a802009616dc220b24cb1a2b53168147fe5cf2ca
  input c1bbf5a4d7e0df9808fa28fb1063354d6a477af3478ce905788f44cfa6911d8e
    nonce: -1
    outputs: []
    versions:
      85e906f8f13be8570c1d01e8d46996461395034f8898faf528112450d6a04045: EscrowID Token CoinVal
  input f4c77feab60e43394a9245e4999c440e321675f7caefd5e84a27b0855e85f0df
    nonce: 3
    outputs: []
    versions:"

# store arguments in a special array 
args=("$@") 
# get number of elements 
ELEMENTS=${#args[@]} 
 
# echo each element in array  
# for loop 
for (( i=0;i<$ELEMENTS;i++)); do 
    echo ${args[${i}]} 
done