import Blockchain.Fae.Contracts
import Blockchain.Fae.Currency

body :: FaeTX (Coin, Maybe (Either (Versioned Coin) (Versioned String))) String
body (_, Nothing) = return ""
body (_, Just (Right (Versioned s))) = return s
