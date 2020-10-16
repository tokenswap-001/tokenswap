-- uniswap && ERC20合约
-- 支持多签合约持有本合约代币的版本
-- add this Contract type when only compile by gluac
type Contract<T> = {
    storage: T
}

-- type State = 'NOT_INITED' | 'COMMON' | 'PAUSED' | 'STOPPED'

let MAX_TOKEN_AMOUNT = 4000000000000000

type Storage = {
    name: string,
    symbol: string,
    supply: int, -- bigint string
    precision: int, -- only used to display
    -- allowed: Map<string>, -- 各用户授权给spender用户可以多次提现的余额，每一项值是一个 userAddress=>amount int构成的json string
    -- lockedAmounts: Map<string>, -- userAddress => "lockedAmount,unlockBlockNumber"
    state: string,
    allowLock: bool,
    fee: int, -- 手续费
    minTransferAmount: int, -- 每次最低转账金额
    feeReceiveAddress: string, -- 手续费接收地址
    admin: string, -- admin user address
	
	------uniswap info
	fee_rate:string, ---兑换token时收取手续费比例  "0.05"
	token_1_contractAddr:string,
	token_2_contractAddr:string,
	token_1_pool_amount:int, --bigint string
	token_2_pool_amount:int, --bigint string
	min_token1_amount:int,
    min_token2_amount:int,
    dao_contract: string -- 治理合约地址
    -- nativeBalances: Map<string, string>, 用户在本合约中拥有的各原生资产的余额，address => json string of assetSymbol => balance amount
}

-- events: Transfer, Paused, Resumed, Stopped, AllowedLock, Locked, Unlocked

var M = Contract<Storage>()

function M:init()
    print("uniswap contract creating")
    self.storage.name = ''
    self.storage.symbol = ''
    self.storage.supply = 0
    self.storage.precision = 0
    self.storage.state = 'NOT_INITED'
    self.storage.admin = caller_address
    self.storage.allowLock = false
    self.storage.fee = 0
    self.storage.minTransferAmount = 0
    self.storage.feeReceiveAddress = caller_address
	
	self.storage.fee_rate = '0.0'
    self.storage.token_1_contractAddr = ''
    self.storage.token_2_contractAddr = ''
    self.storage.token_1_pool_amount = 0
	self.storage.token_2_pool_amount = 0
	self.storage.min_token1_amount = 0
    self.storage.min_token2_amount = 0
    self.storage.dao_contract = ''
	
    print("uniswap contract created")
end

let function getInputPrice(input_amount:int, input_reserve:int, output_reserve:int)
	let a = safemath.safenumber(input_reserve)
	let b = safemath.safenumber(output_reserve)
	let x = safemath.safenumber(input_amount)
	let n = safemath.number_multiply(x, b)
	let r = safemath.number_toint(safemath.number_div(n, safemath.number_add(a, x)))
	return r
end

let function get_from_address()
    -- 支持合约作为代币持有者
    var from_address: string
    let prev_contract_id = get_prev_call_frame_contract_address()
    if prev_contract_id and is_valid_contract_address(prev_contract_id) then
        -- 如果来源方是合约时
        from_address = prev_contract_id
    else
        from_address = caller_address
    end
    return from_address
end

let function checkAdmin(self: table)
    if self.storage.admin ~= get_from_address() then
        return error("you are not admin, can't call this function")
    end
end


-- parse a,b,c format string to [a,b,c]
let function parse_args(arg: string, count: int, error_msg: string)
    if not arg then
        return error(error_msg)
    end
    let parsed = string.split(arg, ',')
    if (not parsed) or (#parsed ~= count) then
        return error(error_msg)
    end
    return parsed
end

let function parse_at_least_args(arg: string, count: int, error_msg: string)
    if not arg then
        return error(error_msg)
    end
    let parsed = string.split(arg, ',')
    if (not parsed) or (#parsed < count) then
        return error(error_msg)
    end
    return parsed
end

let function arrayContains(col: Array<object>, item: object)
    if not item then
        return false
    end
    var value: object
    for _, value in ipairs(col) do
        if value == item then
            return true
        end
    end
    return false
end


let function checkState(self: table)
    if self.storage.state == 'NOT_INITED' then
        return error("contract not inited")
    end
    if self.storage.state == 'PAUSED' then
        return error("contract paused")
    end
    if self.storage.state == 'STOPPED' then
        return error("contract stopped")
    end
end

let function checkStateInited(self: table)
    if self.storage.state == 'NOT_INITED' then
        return error("contract token not inited")
    end
end

let function checkAddress(addr: string)
    let result = is_valid_address(addr)
    if not result then
        return error("address format error")
    end
    return result
end

let function is_native_asset_symbol(token_address: string)
    let len = #token_address
    if not token_address then
        return false
    end
    if len < tointeger(1) then
        return false
    end
    if tointeger(15) < len then
        return false
    end
    return true
end


-- 修改用户在合约中某原生资产的余额，如果余额不够减少，则报错退出
let function changeUserNativeBalance(self: table, address: string, symbol: string, change: int)
    if change == 0 then
        return
    end
    let userAssets: table = json.loads(fast_map_get('nativeBalances', address) or '{}') or {}
    let oldBalance = tointeger(userAssets[symbol] or 0)
    let newBalance = 0
    if change < 0 then
        let amount = - change
        if oldBalance < amount then
            return error("amount exceed balance")
        end
        newBalance = oldBalance - amount
    else
        newBalance = oldBalance + change
    end
    userAssets[symbol] = newBalance
    let newUserAssetsStr = json.dumps(userAssets)
    fast_map_set('nativeBalances', address, newUserAssetsStr)
    let nativeTransferEventArg = json.dumps({
        address: address,
        symbol: symbol,
        change: change,
        reason: ''
    })
    emit NativeBalanceChange(nativeTransferEventArg)
end


let function withdraw_native_asset_private(self:table,from:string,symbol:string,amountStr:string)
    checkState(self)	

    let amount = tointeger(amountStr)

    if (not symbol) or (#symbol < 1) or (not amount) or (amount <= 0) then
        return error("invalid params")
    end
    let fromAddress = from
    let userAssets: table = json.loads(fast_map_get('nativeBalances', fromAddress) or '{}') or {}
    let oldBalance = tointeger(userAssets[symbol] or 0)
    if oldBalance < amount then
        return error("amount exceed balance")
    end
    let newBalance = oldBalance - amount
    userAssets[symbol] = newBalance
    let newUserAssetsStr = json.dumps(userAssets)

    fast_map_set('nativeBalances', fromAddress, newUserAssetsStr)
    -- 从合约中提现

    let res1 = transfer_from_contract_to_address(fromAddress, symbol, amount)
	if res1 ~= 0 then
		return error("transfer asset " .. symbol .. " to " .. fromAddress .. " amount:"..tostring(amount).." error, error code: " .. tostring(res1))
    end	
    
    let nativeTransferEventArg = json.dumps({
        address: fromAddress,
        symbol: symbol,
        change: - amount,
        reason: 'withdraw'
    })
    emit NativeBalanceChange(nativeTransferEventArg)

end


-- 兑换合约的实际逻辑
let function exchangePrivate(self: table, from_address: string, arg: string)
    checkState(self)
	let parsed = parse_at_least_args(arg, 5, "argument format error, need format is sell_token_addr,sell_token_amount,want_buy_token_addr,min_want_buy_token_amount,expired_blocknum,withdraw(optional)")
	
	let sell_token_addr = tostring(parsed[1])
    var sell_token_amount:int = tointeger(parsed[2])
	let want_buy_token_addr = tostring(parsed[3])
	let want_buy_token_amount = tointeger(parsed[4])
    let expired_blocknum = tointeger(parsed[5])
    let withdraw:bool = false
    if #parsed == tointeger(6) then
        withdraw = toboolean(parsed[6])
    end

	if((sell_token_amount<=0)or(want_buy_token_amount<=0)) then
		return error("argument format error, sell_token_amount or min_want_buy_token_amount > 0")
	end
		
	if(expired_blocknum <= 0) then
		return error("expired_blocknum must >= 0")
	end
	
	if(expired_blocknum <= get_header_block_num()) then
		return error("expired_blocknum must > head_block_num")
	end
	
	var token_1_pool_amount = self.storage.token_1_pool_amount
	var token_2_pool_amount = self.storage.token_2_pool_amount
	
	if(token_1_pool_amount <= 0) or (token_2_pool_amount <= 0) then
		return error("pool is empty")
	end
	
	let input_sell_token_amount = sell_token_amount
	sell_token_amount = sell_token_amount
	
	let supply = self.storage.supply
	let feeRate = self.storage.fee_rate
	let nFeeRate = safemath.safenumber(feeRate)
	let temp = safemath.number_multiply(safemath.safenumber(sell_token_amount),nFeeRate)
	var fee:int = safemath.number_toint(temp)
	if (safemath.number_ne(safemath.safenumber(fee), temp)) then
		fee = fee + 1
	end
	
	if (fee <= 0) then
		fee = 1
	end
	
	if(sell_token_amount <= fee) then
		return error("can't get any")
	end
	
	let token_1_contractAddr = self.storage.token_1_contractAddr
	let token_2_contractAddr = self.storage.token_2_contractAddr
	let event_arg = {}
	var get_token_amount:int = 0
	if((sell_token_addr==token_1_contractAddr) and (want_buy_token_addr==token_2_contractAddr)) then
		if (want_buy_token_amount >= token_2_pool_amount) then
			return error("pool amount not enough")
		end
		get_token_amount = getInputPrice(sell_token_amount-fee, token_1_pool_amount, token_2_pool_amount)

		if (get_token_amount <= 0) then
			return error("get_token2_amount <= 0")
		end
		if (want_buy_token_amount > get_token_amount) then
			return error("can't get what you want amount")
		end
		token_1_pool_amount = token_1_pool_amount + sell_token_amount
		token_2_pool_amount = token_2_pool_amount - get_token_amount
		event_arg["buy_asset"] = token_2_contractAddr
		
	elseif((sell_token_addr==token_2_contractAddr) and (want_buy_token_addr==token_1_contractAddr)) then
		if (want_buy_token_amount >= token_1_pool_amount) then
			return error("pool amount not enough")
		end
		get_token_amount = getInputPrice(sell_token_amount-fee, token_2_pool_amount, token_1_pool_amount)

		if (get_token_amount <= 0) then
			return error("get_token1_amount <= 0")
		end
		if (want_buy_token_amount > get_token_amount) then
			return error("can't get what you want amount")
		end
		token_2_pool_amount = token_2_pool_amount + sell_token_amount
		token_1_pool_amount = token_1_pool_amount - get_token_amount
		event_arg["buy_asset"] = token_1_contractAddr
	else
		return error("token address not match for exchange")
	end
	
	if (token_1_pool_amount <= 0) or (token_2_pool_amount <= 0) then
		return error("caculate internal error")
    end
    
    -- 如果是原生资产，则需要从本合约中记录的此用户的余额减少或者增加
	
    --sub from_address balance
    if is_native_asset_symbol(sell_token_addr) then
        -- 卖出资产是原生代币
        changeUserNativeBalance(self, from_address, sell_token_addr, - sell_token_amount)
    else
        -- 卖出资产是合约代币

	    let transfer_from_contract = import_contract_from_address(sell_token_addr)
        let cur_contract = get_current_contract_address()
        let prifixstr = from_address ..","..cur_contract..","
        transfer_from_contract:transferFrom(prifixstr..tostring(sell_token_amount))
    end
	
    -- transfer to from_address
    let buy_asset_symbol = tostring(event_arg["buy_asset"])
    if is_native_asset_symbol(buy_asset_symbol) then
        -- 买入资产是原生代币

        changeUserNativeBalance(self, from_address, buy_asset_symbol, get_token_amount)
        if withdraw  then
            withdraw_native_asset_private(self,from_address,buy_asset_symbol,tostring(get_token_amount))
        end
        
    else
        -- 买入资产是合约代币
        let to_contract = import_contract_from_address(tostring(event_arg["buy_asset"]))
    to_contract:transfer(from_address..","..tostring(get_token_amount))
    end
	
	--set
	self.storage.token_1_pool_amount = token_1_pool_amount
	self.storage.token_2_pool_amount = token_2_pool_amount
	
	event_arg["addr"] = from_address
	event_arg["fee"] = fee -- fee symbol ->sell_asset
	event_arg["sell_asset"] = sell_token_addr
	event_arg["sell_amount"] = sell_token_amount
	event_arg["buy_amount"] = get_token_amount
	emit Exchanged(json.dumps(event_arg))
	
	let get_token_amount_str = tostring(get_token_amount)
	return get_token_amount_str
end
-- call dao 
let function call_dao_contract(self: table,from: string,to: string,amount:int)
    if self.storage.dao_contract == '' then
        return error('dao contract not set')
    end
    let dao_contract = import_contract_from_address(self.storage.dao_contract)
    if from == '' then
        from = 'mint'
    end
    if to == '' then
        to = "destory"
    end
    dao_contract:updateAddressLiquidity(self.id..","..from..","..to..","..tostring(amount))
end

-- 充值原生资产到本合约
function M:on_deposit_asset(jsonstrArgs: string)
    -- return error("not support deposit")
    checkState(self)	
	let arg = json.loads(jsonstrArgs)
    let amount = tointeger(arg.num)
    let symbol = tostring(arg.symbol)
    let param = tostring(arg.param)
	if (not amount) or (amount < 0) then
		 return error("deposit should greater than 0")
	end
	if (not symbol) or (#symbol < 1) then
		 return error("on_deposit_asset arg wrong")
    end
    
    -- 只能支持交易对中的原生资产
    if (symbol ~= self.storage.token_1_contractAddr) and (symbol ~= self.storage.token_2_contractAddr) then
        return error("only support deposit exchange pair assets")
    end
	
    let fromAddress = get_from_address()
    -- 在本合约中记录此用户拥有的此种原生资产增加部分余额
    let userAssets: table = json.loads(fast_map_get('nativeBalances', fromAddress) or '{}') or {}
    let oldBalance = tointeger(userAssets[symbol] or 0)
    let newBalance = oldBalance + amount
    userAssets[symbol] = newBalance
    let newUserAssetsStr = json.dumps(userAssets)
    fast_map_set('nativeBalances', fromAddress, newUserAssetsStr)

    let nativeTransferEventArg = json.dumps({
        address: fromAddress,
        symbol: symbol,
        change: amount,
        reason: 'deposit'
    })
    emit NativeBalanceChange(nativeTransferEventArg)
    -- 如果有params是exchange的参数格式，就直接兑换
    if #param > 0 then
        let parsedParam = string.split(param, ',')
        if #parsedParam >= tointeger(5) then
            return exchangePrivate(self, fromAddress, param)
        end
    end
end

-- 提现原生资产. arg: assetSymbol,amount(without-precision)
function M:withdraw_native_asset(arg: string)
    -- 用户可以从本合约中记录的此用户拥有的余额中提现部分
    checkState(self)	
    let parsedArgs = parse_at_least_args(arg, 2)
    let symbol = tostring(parsedArgs[1])
    let amount = tointeger(parsedArgs[2])

    if (not symbol) or (#symbol < 1) or (not amount) or (amount <= 0) then
        return error("invalid params")
    end
    let fromAddress = get_from_address()
    let userAssets: table = json.loads(fast_map_get('nativeBalances', fromAddress) or '{}') or {}
    let oldBalance = tointeger(userAssets[symbol] or 0)

    if oldBalance < amount then
        return error("amount exceed balance")
    end
    let newBalance = oldBalance - amount
    userAssets[symbol] = newBalance
    let newUserAssetsStr = json.dumps(userAssets)
    fast_map_set('nativeBalances', fromAddress, newUserAssetsStr)
    -- 从合约中提现
    let res1 = transfer_from_contract_to_address(fromAddress, symbol, amount)
	if res1 ~= 0 then
		return error("transfer asset " .. symbol .. " to " .. fromAddress .. " amount:"..tostring(amount).." error, error code: " .. tostring(res1))
    end	
    
    let nativeTransferEventArg = json.dumps({
        address: fromAddress,
        symbol: symbol,
        change: - amount,
        reason: 'withdraw'
    })
    emit NativeBalanceChange(nativeTransferEventArg)
end

-- 查询用户在合约中的原生资产余额
offline function M:query_native_asset(address: string)
    if not is_valid_address(address) then
        return error("invalid address param format")
    end
    let userAssets: table = json.loads(fast_map_get('nativeBalances', address) or '{}') or {}
    return userAssets
end

-- arg: token1_addr,token2_addr,min_token1_amount,min_token2_amount,fee_rate,liquidity_token_name,liquidity_token_symbol
function M:init_config(arg: string)
    checkAdmin(self)
    if self.storage.state ~= 'NOT_INITED' then
        return error("this contract inited before")
    end
    let parsed = parse_args(arg, 7, "arg format error, need format: token1_addr,token2_addr,min_token1_amount,min_token2_amount,fee_rate,liquidity_token_name,liquidity_token_symbol")
    let info = {token1_addr: parsed[1],token2_addr: parsed[2],min_token1_amount: parsed[3],min_token2_amount: parsed[4],fee_rate: parsed[5], liquidity_token_name: parsed[6],liquidity_token_symbol: parsed[7]}
    
    -- 同时支持原生资产和合约代币

    let token_1_contractAddr = tostring(info.token1_addr)
    if not is_native_asset_symbol(token_1_contractAddr) then
        -- 如果是合约代币，需要检查合约API

        let tokenContr1 = import_contract_from_address(token_1_contractAddr)
        if not tokenContr1 or (not tokenContr1.transferFrom) then
            return error("token1_addr not token contract")
        end
    end
    let token_2_contractAddr = tostring(info.token2_addr)
	if (token_2_contractAddr == token_1_contractAddr) then
		return error("token_2_contractAddr and token_1_contractAddr is same")
    end
    if not is_native_asset_symbol(token_2_contractAddr) then
        -- 如果是合约代币，需要检查合约API
        print(token_2_contractAddr, ' is contract token')
        let tokenContr2 = import_contract_from_address(token_2_contractAddr)
        if not tokenContr2 or (not tokenContr2.transferFrom) then
            return error("token1_addr not token contract")
        end
    end
	let min_token1_amount = tointeger(info.min_token1_amount)
	let min_token2_amount = tointeger(info.min_token2_amount)
	
	if((min_token1_amount<=0) or (min_token2_amount<=0)) then
		return error("argument error, min_token_amount to add liquidity must be positive integer")
	end
	
	let fee_rate = tostring(info.fee_rate)
	let safenumber_fee_rate = safemath.safenumber(fee_rate)
	if (safemath.number_lt(safenumber_fee_rate,safemath.safenumber(0)) or safemath.number_gte(safenumber_fee_rate,safemath.safenumber(1))) then
		return error("fee rate must be >=0 and < 1")
	end
	
	let liquidity_token_name = tostring(info.liquidity_token_name)
	if (#liquidity_token_name < 1) then
		return error("liquidity_token_name is empty")
	end
	
	let liquidity_token_symbol = tostring(info.liquidity_token_symbol)
	
	if (#liquidity_token_symbol < 1) then
		return error("liquidity_token_symbol is empty")
	end

	self.storage.name = liquidity_token_name
	self.storage.symbol = liquidity_token_symbol
	self.storage.precision = 1
	self.storage.token_1_contractAddr = token_1_contractAddr
	self.storage.token_2_contractAddr = token_2_contractAddr
	
	self.storage.min_token1_amount = min_token1_amount
	self.storage.min_token2_amount = min_token2_amount

	self.storage.fee_rate = fee_rate
	self.storage.state = 'COMMON'

	info["state"] = 'COMMON'
	info["precision"] = 1
	pprint('info4:', info)
    let eventArgStr = json.dumps(info)
	
    emit Inited(eventArgStr)
end

offline function M:state(arg: string)
    return self.storage.state
end

offline function M:tokenName(arg: string)
    checkStateInited(self)
    return self.storage.name
end

offline function M:precision(_: string)
    checkStateInited(self)
    return self.storage.precision
end

offline function M:tokenSymbol(arg: string)
    checkStateInited(self)
    return self.storage.symbol
end

offline function M:admin(_: string)
    checkStateInited(self)
    return self.storage.admin
end

offline function M:totalSupply(arg: string)
    checkStateInited(self)
    return self.storage.supply
end

offline function M:isAllowLock(_: string)
    let resultStr = tostring(self.storage.allowLock)
    return resultStr
end

offline function M:fee(_: string)
    let feeStr = tostring(self.storage.fee)
    return feeStr
end

offline function M:minTransferAmount(_: string)
    let minTransferAmountStr = tostring(self.storage.minTransferAmount)
    return minTransferAmountStr
end

offline function M:feeReceiveAddress(_: string)
    return self.storage.feeReceiveAddress
end

function M:setFee(feeStr: string)
    checkAdmin(self)
    checkState(self)
    let fee = tointeger(feeStr)
    if (fee ~= 0) and ((not fee) or (fee < 0)) then
        return error("error fee format")
    end
    self.storage.fee = fee
    emit FeeChanged(feeStr)
end

function M:setMinTransferAmount(minTransferAmountStr: string)
    checkAdmin(self)
    checkState(self)
    let minTransferAmount = tointeger(minTransferAmountStr)
    if (minTransferAmount ~= 0) and ((not minTransferAmount) or (minTransferAmount < 0)) then
        return error("error minTransferAmount format")
    end
    self.storage.minTransferAmount = minTransferAmount
    emit MinTransferAmountChanged(minTransferAmountStr)
end

function M:setFeeReceiveAddress(feeReceiveAddress: string)
    checkAdmin(self)
    checkState(self)
    if not is_valid_address(feeReceiveAddress) then
        return error("invalid address")
    end
    if is_valid_contract_address(feeReceiveAddress) then
        return error("can't use contract address")
    end
    self.storage.feeReceiveAddress = feeReceiveAddress
    emit FeeReceiveAddressChanged(feeReceiveAddress)
end


function M:openAllowLock(_: string)
    checkAdmin(self)
    checkState(self)
    if self.storage.allowLock then
        return error("this contract had been opened allowLock before")
    end
    self.storage.allowLock = true
    emit AllowedLock("")
end

let function getBalanceOfUser(self: table, addr: string)
    return tointeger(fast_map_get('users', addr) or 0)
end

offline function M:balanceOf(owner: string)
    checkStateInited(self)
    if (not owner) or (#owner < 1) then
        return error('arg error, need owner address as argument')
    end
    checkAddress(owner)
    let amount = getBalanceOfUser(self, owner)
    let amountStr = tostring(amount)
    return amountStr
end

-- arg: limit(1-based),offset(0-based)}
offline function M:users(arg: string)
    return error("not implemented, you can find users from contract transaction history")
end

-- arg: to_address,integer_amount[,memo]
function M:transfer(arg: string)
    checkState(self)
    let parsed = parse_at_least_args(arg, 2, "argument format error, need format is to_address,integer_amount[,memo]")
    let info = {to: parsed[1], amount: tointeger(parsed[2])}
    let to = tostring(info.to)
    let amount = tointeger(info.amount)
    var memo: string = nil
    if #parsed >= 3 then
        memo = tostring(parsed[3])
    end
    if (not to) or (#to < 1) then
        return error("to address format error")
    end
    let fee = self.storage.fee
    let minTransferAmount = self.storage.minTransferAmount
    let feeReceiveAddress = self.storage.feeReceiveAddress
    if (not amount) or (amount < 1) then
        return error("amount format error")
    end
    if amount <= fee then
        return error("amount not enough for fee")
    end
    if amount < minTransferAmount then
        return error("only transfer amount >= " .. tostring(minTransferAmount) .. " will be accepted")
    end
    checkAddress(to)
    let from_address = get_from_address()
    var from_address_balance = tointeger(fast_map_get('users', from_address) or 0)
    if (not from_address_balance) or (from_address_balance < amount) then
        return error("you have not enoungh amount to transfer out")
    end
    from_address_balance = from_address_balance - amount
    fast_map_set('users', from_address, from_address_balance)
    if from_address_balance == 0 then
        fast_map_set('users', from_address, nil)
    end
    let to_balance = tointeger(fast_map_get('users', to) or 0)
    if (to_balance + amount) < 0 then
        return error("amount overflow")
    end
    fast_map_set('users', to, to_balance + amount - fee)
    if fee > 0 then
        let feeReceiveAddressOldBalance = tointeger(fast_map_get('users', feeReceiveAddress) or 0)
        if (feeReceiveAddressOldBalance + fee) < 0 then
            return error("amount overflow")
        end
        fast_map_set('users', feeReceiveAddress, feeReceiveAddressOldBalance + fee)
    end
    let eventArgStr = json.dumps({from: from_address, to: to, amount: amount - fee, fee: fee, memo: memo})
    emit Transfer(eventArgStr)
    call_dao_contract(self,from_address,to,amount-fee)

	if is_valid_contract_address(to) then
        -- 如果目标是合约地址，可能是多签合约，需要调用回调函数
        let multiOwnedContract = import_contract_from_address(to)
        let amountStr = tostring(amount - fee)
        if multiOwnedContract and (multiOwnedContract.on_deposit_contract_token) then
            multiOwnedContract:on_deposit_contract_token(amountStr)
        end
    end
end

-- spender用户从授权人授权的金额中发起转账
-- arg format: fromAddress,toAddress,amount(with precision)
function M:transferFrom(arg: string)
    checkState(self)
    let parsed = parse_at_least_args(arg, 3, "argument format error, need format is fromAddress,toAddress,amount(with precision)")
    let fromAddress = tostring(parsed[1])
    let toAddress = tostring(parsed[2])
    let amount = tointeger(parsed[3])
    var memo: string = nil
    if #parsed >= 4 then
        memo = tostring(parsed[4])
    end
    checkAddress(fromAddress)
    checkAddress(toAddress)
    if (not amount) or (amount <= 0) then
        return error("amount must be positive integer")
    end
    
    let fee = self.storage.fee
    let minTransferAmount = self.storage.minTransferAmount
    let feeReceiveAddress = self.storage.feeReceiveAddress
    if amount <= fee then
        return error("amount not enough for fee")
    end
    if amount < minTransferAmount then
        return error("only transfer amount >= " .. tostring(minTransferAmount) .. " will be accepted")
    end

    let from_address_balance = tointeger(fast_map_get('users', fromAddress) or 0)
    if (not from_address_balance) or (amount > from_address_balance) then
        return error("fromAddress not have enough token to withdraw")
    end
    let allowedDataStr = fast_map_get('allowed', fromAddress)
    if (not allowedDataStr) then
        return error("not enough approved amount to withdraw")
    end
    let allowedData: Map<int> = totable(json.loads(tostring(allowedDataStr)))
    let contractCaller = get_from_address()
    if (not allowedData) or (not allowedData[contractCaller]) then
        return error("not enough approved amount to withdraw")
    end
    let approvedAmount = tointeger(allowedData[contractCaller])
    if (not approvedAmount) or (amount > approvedAmount) then
        return error("not enough approved amount to withdraw")
    end
    let toAddressOldBalance = tointeger(fast_map_get('users', toAddress) or 0)
    if (toAddressOldBalance + amount) < 0 then
        return error("amount overflow")
    end
    fast_map_set('users', toAddress, toAddressOldBalance + amount - fee)
    if fee > 0 then
        let feeReceiveAddressOldBalance = tointeger(fast_map_get('users', feeReceiveAddress) or 0)
        if (feeReceiveAddressOldBalance + fee) < 0 then
            return error("amount overflow")
        end
        fast_map_set('users', feeReceiveAddress, feeReceiveAddressOldBalance + fee)
    end
    fast_map_set('users', fromAddress, tointeger(fast_map_get('users', fromAddress)) - amount)
    if tointeger(fast_map_get('users', fromAddress)) == 0 then
        fast_map_set('users', fromAddress, nil)
    end
    allowedData[contractCaller] = approvedAmount - amount
    if allowedData[contractCaller] == 0 then
        allowedData[contractCaller] = nil
    end
    fast_map_set('allowed', fromAddress, json.dumps(allowedData))
    let eventArgStr = json.dumps({from: fromAddress, to: toAddress, amount: amount - fee, fee: fee, memo: memo})
    emit Transfer(eventArgStr)
    call_dao_contract(self,fromAddress,toAddress,amount-fee)
end

-- 授权另一个用户可以从自己的余额中提现
-- arg format: spenderAddress,amount(with precision)
function M:approve(arg: string)
    checkState(self)
    let parsed = parse_at_least_args(arg, 2, "argument format error, need format is spenderAddress,amount(with precision)")
    let spender = tostring(parsed[1])
    checkAddress(spender)
    let amount = tointeger(parsed[2])
    if (not amount) or (amount < 0) then
        return error("amount must be non-negative integer")
    end
    var allowedData: Map<int>
    let contractCaller = get_from_address()
    if (not fast_map_get('allowed', contractCaller)) then
        allowedData = {}
    else
        allowedData = totable(json.loads(tostring(fast_map_get('allowed', contractCaller))))
        if not allowedData then
            return error("allowed storage data error")
        end
    end
    allowedData[spender] = amount
    fast_map_set('allowed', contractCaller, json.dumps(allowedData))
    let eventArg = {from: contractCaller, spender: spender, amount: amount}
    emit Approved(json.dumps(eventArg))
end

-- 查询一个用户被另外某个用户授权的金额
-- arg format: spenderAddress,authorizerAddress
offline function M:approvedBalanceFrom(arg: string)
    let parsed = parse_at_least_args(arg, 2, "argument format error, need format is spenderAddress,authorizerAddress")
    let spender = tostring(parsed[1])
    let authorizer = tostring(parsed[2])
    checkAddress(spender)
    checkAddress(authorizer)
    let allowedDataStr = fast_map_get('allowed', authorizer)
    if (not allowedDataStr) then
        return "0"
    end
    let allowedData: Map<int> = totable(json.loads(tostring(allowedDataStr)))
    if (not allowedData) then
        return "0"
    end
    let allowedAmount = allowedData[spender]
    if (not allowedAmount) then
        return "0"
    end
    let allowedAmountStr = tostring(allowedAmount)
    return allowedAmountStr
end

-- 查询用户授权给其他人的所有金额
-- arg format: fromAddress
offline function M:allApprovedFromUser(arg: string)
    let authorizer = arg
    checkAddress(authorizer)
    let allowedDataStr = fast_map_get('allowed', authorizer)
    if (not allowedDataStr) then
        return "{}"
    end
    return allowedDataStr
end

function M:pause(arg: string)
    if self.storage.state == 'STOPPED' then
        return error("this contract stopped now, can't pause")
    end
    if self.storage.state == 'PAUSED' then
        return error("this contract paused now, can't pause")
    end
    checkAdmin(self)
    self.storage.state = 'PAUSED'
    emit Paused("")
end

function M:resume(arg: string)
    if self.storage.state ~= 'PAUSED' then
        return error("this contract not paused now, can't resume")
    end
    checkAdmin(self)
    self.storage.state = 'COMMON'
    emit Resumed("")
end

function M:stop(arg: string)
    if self.storage.state == 'STOPPED' then
        return error("this contract stopped now, can't stop")
    end
    if self.storage.state == 'PAUSED' then
        return error("this contract paused now, can't stop")
    end
    checkAdmin(self)
    self.storage.state = 'STOPPED'
    emit Stopped("")
end

-- arg: integer_amount,unlockBlockNumber
function M:lock(arg: string)
    checkState(self)
    if (not self.storage.allowLock) then
        return error("this token contract not allow lock balance")
    end
    let parsed = parse_args(arg, 2, "arg format error, need format is integer_amount,unlockBlockNumber")
    let toLockAmount = tointeger(parsed[1])
    let unlockBlockNumber = tointeger(parsed[2])
    if (not toLockAmount) or (toLockAmount<1) then
        return error("to unlock amount must be positive integer")
    end
    if (not unlockBlockNumber) or (unlockBlockNumber < get_header_block_num()) then
        return error("to unlock block number can't be earlier than current block number " .. tostring(get_header_block_num()))
    end
    let from_address = get_from_address()
    if from_address ~= caller_address then
        return error("only common user account can lock balance") -- 只有普通账户可以锁仓，合约不能锁仓
    end
    let balance = getBalanceOfUser(self, from_address)
    if (toLockAmount > balance) then
        return error("you have not enough balance to lock")
    end
    if (not fast_map_get('lockedAmounts', from_address)) then
        fast_map_set('lockedAmounts', from_address, tostring(toLockAmount) .. ',' .. tostring(unlockBlockNumber))
    else
        return error("you have locked balance now, before lock again, you need unlock them or use other address to lock")
    end
    fast_map_set('users', from_address, balance - toLockAmount)
    emit Locked(tostring(toLockAmount))
end

function M:unlock(_: string)
    checkState(self)
    if (not self.storage.allowLock) then
        return error("this token contract not allow lock balance")
    end
    let from_address = get_from_address()
    if (not fast_map_get('lockedAmounts', from_address)) then
        return error("you have not locked balance")
    end
    let lockedInfoParsed = parse_args(tostring(fast_map_get('lockedAmounts', from_address)), 2, "locked amount info format error")
    let lockedAmount = tointeger(lockedInfoParsed[1])
    let canUnlockBlockNumber = tointeger(lockedInfoParsed[2])

    if (get_header_block_num() < canUnlockBlockNumber) then
        return error("your locked balance only can be unlock after block #" .. tostring(canUnlockBlockNumber))
    end
    fast_map_set('lockedAmounts', from_address, nil)
    let fromAddressOldBalance = getBalanceOfUser(self, from_address)
    if (fromAddressOldBalance + lockedAmount) < 0 then
        return error("amount overflow")
    end
    fast_map_set('users', from_address, fromAddressOldBalance + lockedAmount)
    emit Unlocked(from_address .. ',' .. tostring(lockedAmount))
end

-- arg: userAddress
-- only admin can call this api
function M:forceUnlock(arg: string)
    checkState(self)
    if (not self.storage.allowLock) then
        return error("this token contract not allow lock balance")
    end
    checkAdmin(self)
    let userAddr = arg
    if (not userAddr) or (#userAddr < 1) then
        return error("argument format error, need format userAddress")
    end
    checkAddress(userAddr)

    if (not fast_map_get('lockedAmounts', userAddr)) then
        return error("this user have not locked balance")
    end
    let lockedInfoParsed = parse_args(tostring(fast_map_get('lockedAmounts', userAddr)), 2, "locked amount info format error")
    let lockedAmount = tointeger(lockedInfoParsed[1])
    let canUnlockBlockNumber = tointeger(lockedInfoParsed[2])

    if (get_header_block_num() < canUnlockBlockNumber) then
        return error("this user locked balance only can be unlock after block #" .. tostring(canUnlockBlockNumber))
    end
    fast_map_set('lockedAmounts', userAddr, nil)
    let userOldBalance = getBalanceOfUser(self, userAddr)
    if (userOldBalance + lockedAmount) < 0 then
        return error("amount overflow")
    end
    fast_map_set('users', userAddr, userOldBalance + lockedAmount)
    emit Unlocked(userAddr .. ',' .. tostring(lockedAmount))
end

offline function M:lockedBalanceOf(owner: string)
    if (not fast_map_get('lockedAmounts', owner)) then
        return '0,0'
    else
        let resultStr = fast_map_get('lockedAmounts', owner)
        return resultStr
    end
end

----------------------uniswap-------------------------------------

-- add_token1_amount,max_add_token2_amount,expired_blocknum
-- 可包含在<=expired_blocknum的block里
function M:addLiquidity(arg: string)
	checkState(self)
	let parsed = parse_at_least_args(arg, 3, "argument format error, need format is add_token1_amount,max_add_token2_amount,expired_blocknum")
    let add_token1_amount = tointeger(parsed[1])
    let max_add_token2_amount = tointeger(parsed[2])
	let expired_blocknum = tointeger(parsed[3])

	if((add_token1_amount<=0)or(max_add_token2_amount<=0)) then
		return error("add token amount must > 0")
	end		
	if(expired_blocknum <= 0) then
		return error("expired_blocknum must >= 0")
	end
	if(add_token1_amount<(self.storage.min_token1_amount)) then
		return error("add_token1_amount must >= min_token1_amount")
	end
	if(max_add_token2_amount<(self.storage.min_token2_amount)) then
		return error("max_add_token2_amount must >= min_token2_amount")
	end	
	if(expired_blocknum <= get_header_block_num()) then
		return error("expired_blocknum must > head_block_num")
	end	
	if((add_token1_amount > MAX_TOKEN_AMOUNT) or (max_add_token2_amount > MAX_TOKEN_AMOUNT)) then
		return error("add_token_amoun must <= "..tostring(MAX_TOKEN_AMOUNT))
	end

	--check pool
	let token_1_pool_amount = self.storage.token_1_pool_amount
	let token_2_pool_amount = self.storage.token_2_pool_amount
	
	if((token_1_pool_amount > MAX_TOKEN_AMOUNT) or (token_2_pool_amount > MAX_TOKEN_AMOUNT)) then
		return error("token_pool_amount exceed MAX_TOKEN_AMOUNT:"..tostring(MAX_TOKEN_AMOUNT))
	end

	let add_token1_amount_safenumber = safemath.safenumber(add_token1_amount)
	var caculate_token2_amount:int = max_add_token2_amount

	let safenumber_0 = safemath.safenumber(0)
	if((token_1_pool_amount~=0) and (token_2_pool_amount~=0)) then
		let temp = safemath.number_div(safemath.number_multiply(add_token1_amount_safenumber,token_2_pool_amount),token_1_pool_amount)
		caculate_token2_amount = tointeger(safemath.number_toint(temp))
		if(safemath.number_ne(safemath.safenumber(caculate_token2_amount),temp)) then
			caculate_token2_amount = caculate_token2_amount + 1
		end
		if(caculate_token2_amount > max_add_token2_amount) then
			return error("caculate_token2_amount > max_add_token2_amount")
		end
	end

	if((MAX_TOKEN_AMOUNT-token_1_pool_amount)< add_token1_amount) then
		return error("after add, token_1_pool_amount exceed MAX_TOKEN_AMOUNT:"..tostring(MAX_TOKEN_AMOUNT))
	end
	
	if((MAX_TOKEN_AMOUNT-token_2_pool_amount)< caculate_token2_amount) then
		return error("after add, token_2_pool_amount exceed MAX_TOKEN_AMOUNT:"..tostring(MAX_TOKEN_AMOUNT))
	end
	
	--transfer balance
	let from_address = get_from_address()
	let cur_contract = get_current_contract_address()
	let prifixstr = from_address ..","..cur_contract..","
	let token_1_contractAddr = self.storage.token_1_contractAddr
    let token_2_contractAddr = self.storage.token_2_contractAddr

    -- 如果是原生资产，从本合约中存有的此用户的此资产余额中扣除
    if is_native_asset_symbol(token_1_contractAddr) then
        -- 如果token1是原生资产
        changeUserNativeBalance(self, from_address, token_1_contractAddr, - add_token1_amount)
    else
        -- 如果token1是合约代币
        let token1_contract = import_contract_from_address(token_1_contractAddr)
        token1_contract:transferFrom(prifixstr..tostring(add_token1_amount))
    end

    if is_native_asset_symbol(token_2_contractAddr) then
        -- 如果token2是原生资产
        changeUserNativeBalance(self, from_address, token_2_contractAddr, - caculate_token2_amount)
    else
        -- 如果token2是合约代币
        let token2_contract = import_contract_from_address(token_2_contractAddr)
        token2_contract:transferFrom(prifixstr..tostring(caculate_token2_amount))
    end

	--mint liquidity token
	var token_amount:int
	let supply = self.storage.supply
	--let supply_safenumber = safemath.safenumber(supply)
	if((token_1_pool_amount==0) and (token_2_pool_amount==0)) then
		token_amount = add_token1_amount*1000
	elseif((token_1_pool_amount~=0) and (token_2_pool_amount~=0)) then
		token_amount = safemath.number_toint(safemath.number_div(safemath.number_multiply(add_token1_amount_safenumber, supply), token_1_pool_amount))
	else
		return error("internal error, token_1_pool_amount,token_2_pool_amount,supply not unified 0")
	end

	if(token_amount <= 0) then
		return error("get liquidity token amount is 0")
	end
	
	let supply_new = supply + token_amount
	if(supply_new - supply ~= token_amount) then
		return error("supply overflow")
	end

	self.storage.token_1_pool_amount = token_1_pool_amount + add_token1_amount
	self.storage.token_2_pool_amount = token_2_pool_amount + caculate_token2_amount

	self.storage.supply = supply_new
	
	--add user liquidity token balance
	let bal = tointeger(fast_map_get("users",from_address) or 0)
	let bal_new = bal + token_amount
	fast_map_set("users",from_address,bal_new)

	let eventArg = {}
	eventArg[token_1_contractAddr] = add_token1_amount
	eventArg[token_2_contractAddr] = caculate_token2_amount
    emit LiquidityAdded(json.dumps(eventArg))
	emit LiquidityTokenMinted(tostring(token_amount))
	let eventArgStr = json.dumps({from: "", to: from_address, amount: token_amount, fee: "0", memo: ""})
    emit Transfer(eventArgStr)
    call_dao_contract(self,"",from_address,token_amount)


end

-- args: destory_liquidity_token_amount,min_remove_asset1_amount,min_remove_asset2_amount,expired_blocknum,withdraw
-- 可包含在<=expired_blocknum的block里
function M:removeLiquidity(arg: string)
	checkState(self)
	let parsed = parse_at_least_args(arg, 4, "argument format error, need format is destory_liquidity_token_amount,min_remove_asset1_amount,min_remove_asset2_amount,expired_blocknum,withdraw(optional)")
	
	let destory_liquidity_token_amount = tointeger(parsed[1])
    let min_remove_token1_amount = tointeger(parsed[2])
	let min_remove_token2_amount = tointeger(parsed[3])
    let expired_blocknum = tointeger(parsed[4])
    let withdraw = false
    if #parsed > tointeger(4) then
        withdraw = toboolean(parsed[5])
    end

	if((min_remove_token1_amount<0)or(min_remove_token2_amount<0)) then
		return error("argument format error, input remove token amount must >= 0")
	end
		
	if(expired_blocknum <= 0) then
		return error("expired_blocknum must >= 0")
	end
	
	if(expired_blocknum <= get_header_block_num()) then
		return error("expired_blocknum must > head_block_num")
	end
	
	--check liquidity token 
	let from_address = get_from_address()
	let bal = tointeger(fast_map_get("users", from_address) or 0)
	if (destory_liquidity_token_amount > bal) then
		return error("you have not enough liquidity to remove")
	end
	
	--check pool
	let token_1_pool_amount = self.storage.token_1_pool_amount
	let token_2_pool_amount = self.storage.token_2_pool_amount
	let supply = self.storage.supply
	
	if ((token_1_pool_amount <= 0) or (supply<=0) or (token_2_pool_amount<= 0)) then
		return error("pool is empty")
	end
	print(min_remove_token1_amount,token_1_pool_amount,min_remove_token2_amount,token_2_pool_amount)
	if ((min_remove_token1_amount > token_1_pool_amount) or (min_remove_token2_amount > token_2_pool_amount)) then
		return error("wrong remove_token_amount")
	end
	
	--calulate
	var caculate_token1_amount:int = 0
	var caculate_token2_amount:int = 0
	
	if (supply == destory_liquidity_token_amount) then
		caculate_token1_amount = token_1_pool_amount
		caculate_token2_amount = token_2_pool_amount
	else
		let tk = safemath.safenumber(destory_liquidity_token_amount)
		let quota = safemath.number_div(tk, supply)
		caculate_token1_amount = safemath.number_toint(safemath.number_multiply(quota, token_1_pool_amount))
		caculate_token2_amount = safemath.number_toint(safemath.number_multiply(quota, token_2_pool_amount))	
	end
	
	if (caculate_token1_amount < min_remove_token1_amount) then
		return error("caculate_token1_amount < min_remove_token1_amount")
	end
	if (caculate_token2_amount < min_remove_token2_amount) then
		return error("caculate_token2_amount < min_remove_token2_amount")
	end
	
	-- transfer token1 token2 to from_address
	let token_1_contractAddr = self.storage.token_1_contractAddr
    let token_2_contractAddr = self.storage.token_2_contractAddr
    
    -- 如果是原生资产，给本合约中此用户的原生资产增加余额
	
    let prifixstr = from_address..","
    
    if is_native_asset_symbol(token_1_contractAddr) then
        -- 如果token1是原生资产
        changeUserNativeBalance(self, from_address, token_1_contractAddr, caculate_token1_amount)
        if withdraw  then
            withdraw_native_asset_private(self,from_address,token_1_contractAddr,tostring(caculate_token1_amount))
        end  
        
    else
        -- 如果token1是合约代币
        let token1_contract = import_contract_from_address(token_1_contractAddr)
        token1_contract:transfer(prifixstr..tostring(caculate_token1_amount))
    end
    
    if is_native_asset_symbol(token_2_contractAddr) then
        -- 如果token2是原生资产
        changeUserNativeBalance(self, from_address, token_2_contractAddr, caculate_token2_amount)
        if withdraw  then
            withdraw_native_asset_private(self,from_address,token_2_contractAddr,tostring(caculate_token2_amount))
        end 
    else
        let token2_contract = import_contract_from_address(token_2_contractAddr)
        token2_contract:transfer(prifixstr..tostring(caculate_token2_amount))
    end
	
	--sub supply, sub token ,sub pool amount
	self.storage.token_1_pool_amount = token_1_pool_amount - caculate_token1_amount
	self.storage.token_2_pool_amount = token_2_pool_amount - caculate_token2_amount
	self.storage.supply = supply - destory_liquidity_token_amount
	fast_map_set("users", from_address,bal-destory_liquidity_token_amount)
	
	let eventArg = {}
	eventArg[token_1_contractAddr] = caculate_token1_amount
	eventArg[token_2_contractAddr] = caculate_token2_amount
    emit LiquidityRemoved(json.dumps(eventArg))
    emit LiquidityTokenDestoryed(tostring(destory_liquidity_token_amount))	
    
	let transferEventArgStr = json.dumps({from: from_address, to: "", amount: destory_liquidity_token_amount, fee: "0", memo: ""})
    emit Transfer(transferEventArgStr)
    call_dao_contract(self,from_address,"",destory_liquidity_token_amount)
end

--args: sell_token_addr,sell_token_amount,want_buy_token_addr,min_want_buy_token_amount,expired_blocknum
function M:exchange(arg: string)
	let from_address = get_from_address()
	return exchangePrivate(self, from_address, arg)
end

--arg: min_token1_amount,min_token2_amount 
function M:setMinAddAmount(arg: string)
	checkState(self)
	checkAdmin(self)
	
	let parsed = parse_at_least_args(arg, 2, "argument format error, need format is min_token1_amount,min_token2_amount")
	
	let min_token1_amount = tointeger(parsed[1])
    let min_token2_amount = tointeger(parsed[2])
	
	if(min_token1_amount<=0) or (min_token2_amount<=0) then
		return error("min_amount must > 0")
	end
	
	self.storage.min_token1_amount = min_token1_amount
	self.storage.min_token2_amount = min_token2_amount
	
	emit SetMinAddAmount(arg)
	
end
-------------------------------
offline function M:getInfo(arg: string)
	let infos = {}
	infos["admin"] = self.storage.admin
	infos["state"] = self.storage.state
	infos["fee_rate"] = self.storage.fee_rate
	infos["token_name"] = self.storage.name
	infos["token_symbol"] = self.storage.symbol
	infos["token_1_contractAddr"] = self.storage.token_1_contractAddr
	infos["token_2_contractAddr"] = self.storage.token_2_contractAddr
	infos["token_1_pool_amount"] = self.storage.token_1_pool_amount
	infos["token_2_pool_amount"] = self.storage.token_2_pool_amount
	infos["min_token1_amount"] = self.storage.min_token1_amount
    infos["min_token2_amount"] = self.storage.min_token2_amount
    infos["dao_contract"] = self.storage.dao_contract
	let r = json.dumps(infos)
    return r
end



--arg: want_sell_token_addr,want_sell_token_amount,want_buy_token_addr
offline function M:caculateExchangeAmount(arg: string)
	checkState(self)
	let parsed = parse_at_least_args(arg, 3, "argument format error, need format is want_sell_token_addr,want_sell_token_amount,want_buy_token_addr")
	
	let want_sell_token_addr = tostring(parsed[1])
    var want_sell_token_amount:int = tointeger(parsed[2])
	let want_buy_token_addr = tostring(parsed[3])
	
	if (want_sell_token_amount<= 0) then
		return error("want_sell_token_amount must > 0")
	end
	

	let token_1_contractAddr = self.storage.token_1_contractAddr
	let token_2_contractAddr = self.storage.token_2_contractAddr
	
	let token_1_pool_amount = self.storage.token_1_pool_amount
	let token_2_pool_amount = self.storage.token_2_pool_amount
	
	if(token_1_pool_amount<=0) or (token_2_pool_amount<=0) then
		return error("pool is empty")
	end
	
	if(want_buy_token_addr == want_sell_token_addr) then
		return error("want_sell_token_addr and want_buy_token_addr is same")
	end
	
	if(want_sell_token_addr~=token_1_contractAddr) and (want_sell_token_addr~=token_2_contractAddr) then
		return error("want_sell_token_addr not match")
	end
	
	if(want_buy_token_addr~=token_1_contractAddr) and (want_buy_token_addr~=token_2_contractAddr) then
		return error("want_buy_token_addr not match")
	end
	
	-- let transfer_from_contract = import_contract_from_address(want_sell_token_addr)

	want_sell_token_amount = want_sell_token_amount
	
	let fee_rate = self.storage.fee_rate
	let nFeeRate = safemath.safenumber(fee_rate)
	let temp = safemath.number_multiply(safemath.safenumber(want_sell_token_amount),nFeeRate)
	var fee:int = safemath.number_toint(temp)
	if(safemath.number_ne(safemath.safenumber(fee),temp)) then
		fee = fee+1
	end
	
	if(fee<=0) then
		fee=1
	end
	
	if(want_sell_token_amount <= fee) then
		return "0"
	end
	
	var get_token_amount:int = 0
	if(want_sell_token_addr==token_1_contractAddr) and (want_buy_token_addr==token_2_contractAddr) then
		get_token_amount = getInputPrice(want_sell_token_amount-fee, token_1_pool_amount, token_2_pool_amount)
	elseif(want_sell_token_addr==token_2_contractAddr) and (want_buy_token_addr==token_1_contractAddr) then
		get_token_amount = getInputPrice(want_sell_token_amount-fee, token_2_pool_amount, token_1_pool_amount)
	else
		return error("input token address not match")
	end
	
	let get_token_amount_str = tostring(get_token_amount)
	return get_token_amount_str
end

--arg: liquidity_tokenAmount
offline function M:caculatePoolShareByToken(arg: string)
	checkState(self)
	let tokenAmount = tointeger(arg)
	
	if (tokenAmount <= 0) then
		return error("input arg must be positive integer");
	end
	let supply = self.storage.supply
	
	if (tokenAmount > supply) then
		return error("input tokenAmount must <= supply");
	end
	
	let token_1_contractAddr = self.storage.token_1_contractAddr
	let token_2_contractAddr = self.storage.token_2_contractAddr
	
	let token_1_pool_amount = self.storage.token_1_pool_amount
	let token_2_pool_amount = self.storage.token_2_pool_amount
	
	let result = {}
	if(tokenAmount == supply) then
		result[token_1_contractAddr] = token_1_pool_amount
		result[token_2_contractAddr] = token_2_pool_amount
	else
		let quota = safemath.number_div(safemath.safenumber(tokenAmount),supply)
		result[token_1_contractAddr] = safemath.number_toint(safemath.number_multiply(quota, token_1_pool_amount))
		result[token_2_contractAddr] = safemath.number_toint(safemath.number_multiply(quota, token_2_pool_amount))
	end
	
	let r = json.dumps(result)
	return r
end


--arg: address
offline function M:getUserRemoveableLiquidity(arg: string)
	checkState(self)
	let addr = arg
	let bal = fast_map_get("users",addr)
	
	let result = {}
	let token_1_contractAddr = self.storage.token_1_contractAddr
	let token_2_contractAddr = self.storage.token_2_contractAddr
	if not bal then
		result[token_1_contractAddr] = 0
		result[token_2_contractAddr] = 0
	else
		let token_balance = tointeger(bal)
		let token_1_pool_amount = self.storage.token_1_pool_amount
		let token_2_pool_amount = self.storage.token_2_pool_amount
		let supply = self.storage.supply
		
		if(token_balance == supply) then
			result[token_1_contractAddr] = token_1_pool_amount
			result[token_2_contractAddr] = token_2_pool_amount
		else
			
			let a = safemath.safenumber(token_1_pool_amount)
			let b = safemath.safenumber(token_2_pool_amount)
			let quota = safemath.number_div(safemath.safenumber(token_balance),safemath.safenumber(supply))
			result[token_1_contractAddr] = safemath.number_toint(safemath.number_multiply(quota, a))
			result[token_2_contractAddr] = safemath.number_toint(safemath.number_multiply(quota, b))
		end		
	end
	
	let r = json.dumps(result)
	return r
end

function M:setDaoContractAddress(owner:string)
    checkState(self)
    checkAdmin(self)
    self.storage.dao_contract = owner
end

offline function M:getDaoContract(_:string)
    checkState(self)
    return self.storage.dao_contract
end

function M:on_destroy()
    error("can't destroy token contract")
end

return M
