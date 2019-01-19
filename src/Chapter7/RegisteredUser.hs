module Chapter7.RegisteredUser where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name) 
                          (AccountNumber acctNum)) = 
  putStrLn $ name ++ " " ++ show acctNum
  
data WherePenguinsLive = 
    Galapagos 
  | Antartica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)
  
data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmieWhereTheyLive :: Penguin -> WherePenguinsLive
gimmieWhereTheyLive (Peng whereItLives) = whereItLives

humboldt = Peng SouthAmerica
gentoo = Peng Antartica
macaroni = Peng Antartica
little = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarticPenguin :: Penguin -> Bool
antarticPenguin (Peng Antartica) = True
antarticPenguin _                = False

antarticOrGalapagos :: Penguin -> Bool
antarticOrGalapagos p = (galapagosPenguin p || antarticPenguin p)