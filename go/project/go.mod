module bitcoin_miner

go 1.15

require (
	bitcoin_miner/hash v0.0.0
	bitcoin_miner/message v0.0.0
)

replace bitcoin_miner/message => ./message
replace bitcoin_miner/hash => ./hash

