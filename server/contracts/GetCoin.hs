import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

body :: FaeTX RewardEscrowID ()
body rID = do
  coin <- reward rID
  deposit coin "self"
