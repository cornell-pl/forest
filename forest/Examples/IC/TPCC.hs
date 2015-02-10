{-# LANGUAGE ExistentialQuantification, RankNTypes, TupleSections, OverlappingInstances, TypeFamilies, StandaloneDeriving, TypeOperators, ConstraintKinds, DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, ViewPatterns #-}

-- The TPC-C transaction benchmark as found in http://www.tpc.org/tpc_documents_current_versions/pdf/tpc-c_v5-11.pdf

module Examples.IC.TPCC where

import Data.Hashable
import Prelude hiding (mod,read,const)
import qualified Prelude
import Control.Monad.Incremental.Adapton
import Control.Monad.Lazy
import Control.Monad.Incremental.Display
import Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.Reader as Reader
import Data.List
import Data.Time
import Data.Char
import Data.Maybe
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.List.Split
import System.Posix.Types
import Data.List as List
import qualified Data.Foldable as Foldable
import Data.Int
import Data.IORef
import Control.Monad.Incremental as Inc hiding (new,read)
import qualified Data.Set as Set
import Control.Monad.State as State
import Control.Exception (Exception(..),SomeException(..))

import System.TimeIt

import Control.Concurrent
import Control.Monad
import System.IO
import System.Directory
import Test.QuickCheck.Gen
import Control.Monad.State (State(..),StateT(..),MonadState(..))
import qualified Control.Monad.State as State
import Control.Monad.Trans
import Data.WithClass.MData
import System.FilePath.Posix
import Data.WithClass.MGenerics
import Control.Monad.Incremental.Draw
import Control.Monad.Incremental.Generics
import Control.Monad.Incremental.List
import System.Mem.MemoTable hiding (memo)
import System.IO.Unsafe
import Language.Pads.Padsc as Pads hiding (numErrors,take)

import Data.Typeable.Internal
import Debug.Trace
import Data.DeepTypeable
import Language.Haskell.TH.Syntax
import Language.Pads.Library.Native

import Language.Forest.IC
import Language.Forest.IC.PadsInstances
import Language.Forest.Pure.MetaData (cleanSymLinkFileInfo,cleanFileInfo)

-- base types
[ipads|
	-- unique id
	type Id = Integer
	-- variable text
	type VText (size :: Int) = StringVW size
	-- fixed text
	type FText (size :: Int) = StringFW size
	type Signed_4 = Int
	type Signed_4_4 = Float
	type Signed_5_2 = Float
	type Signed_6_2 = Float
	type Signed_12_2 = Double
	type Unsigned_2 = Word8
	type Unsigned_4 = Word8
	type Unsigned_8 = Word16
	type DateTime = DateFSE <| ("%Y-%m-%d", RE "$") |>
|]

[ipads|
	data Location = Location {
		  locationStreet :: Line ( [ VText 20 | ';' ] length 2)
		, locationCity :: Line (VText 20)
		, locationState :: Line (FText 2)
		, locationZip :: (FText 9)
	}
	
	data WarehouseInfo = WarehouseInfo {
		  warehouseName :: Line (VText 10)
		, warehouseLocation :: Line Location
		, warehouseTax :: Line Signed_4_4
		, warehouseYTD :: Signed_12_2
	}
	
	data DistrictInfo = DistrictInfo {
		  districtName :: Line (VText 10)
		, districtLocation :: Line Location
		, districtTax :: Line Signed_4_4
		, districtYTD :: Line Signed_12_2
		, districtNextOrder :: Id
	}
	
	data Person = Person {
		  firstName :: Line (VText 16)
		, middleName :: Line (FText 2)
		, lastName :: VText 12
	}
	
	data CustomerInfo = CustomerInfo {
		  customerName :: Line Person
		, customerLocation :: Line Location
		, customerPhone :: Line (FText 16)
		, customerSince :: Line DateTime
		, customerCredit :: Line (FText 2)
		, customerCreditLimit :: Line Signed_12_2
		, customerDiscount :: Line Signed_4_4
		, customerBalance :: Line Signed_12_2
		, customerYTDPayment :: Line Signed_12_2
		, customerPaymentCount :: Line Unsigned_4
		, customerDeliveryCount :: Line Unsigned_4
		, customerData :: VText 500
	}
	
	data OrderInfo (size :: Unsigned_2) = OrderInfo {
		  orderEntryDate :: Line DateTime
		, orderCarrier :: Line (Maybe Id)
		, orderLinesCount :: Line Unsigned_2 where <| orderLinesCount == size |>
		, orderAllLocal :: Bool
	}
	
	data OrderLineInfo = OrderLineInfo {
		  orderLineNumber :: Line Id
		, orderLineDeliveryDate :: Line (Maybe DateTime)
		, orderLineQuantity :: Line Unsigned_2
		, orderLineAmount :: Line Signed_6_2
		, orderLineDistInfo :: FText 24
	}
	
	data ItemInfo = ItemInfo {
		  itemImage :: Line String
		, itemName :: Line (VText 24)
		, itemPrice :: Line (Signed_5_2)
		, itemData :: VText 50
	}
	
	data StockItemInfo = StockItemInfo {
		  stockItemQuantity :: Line Signed_4
		, stockItemDists :: [ FText 24 | ";" ] length 10
		, stockItemYTD :: Line Unsigned_8
		, stockItemOrderCount :: Line Unsigned_4
		, stockItemRemoteCount :: Line Unsigned_4
		, stockItemData :: VText 50
	}
	
	newtype HistoryTable = HistoryTable ([Line HistoryEntry] terminator EOF)
	
	data HistoryEntry = HistoryEntry {
		        historyCustomer :: Id
		, ',' , historyCustomerDistrict :: Id
		, ',' , historyCustomerWarehouse :: Id
		, ',' , historyDistrict :: Id
		, ',' , historyWarehouse :: Id
		, ',' , historyDate :: DateTime
		, ',' , historyAmount :: Signed_6_2
		, ',' , historyData :: VText 24
	}
|]

[iforest|
	type Company = Directory {
		  warehouses :: Map [ w :: Warehouse | w :: Id <- matches (GL "*") ]
		, items :: Map [ i :: Item | i :: Id <- matches (GL "*") ]
		, history :: History
	}
	
	-- since the table has no primary key, we store it to a file
	type History = File HistoryTable

	type Warehouse = Directory {
		  warehouseInfo :: File WarehouseInfo
		, districts :: Map [ d :: District | d :: Id <- matches (GL "*") ]
		, stock :: Map [ s :: StockItem | s :: Id <- matches (GL "*") ]
	}
	
	type Item = File ItemInfo
	
	type District = Directory {
		  districtInfo :: File DistrictInfo
		, customers :: Map [ c :: Customer | c :: Id <- matches (GL "*") ]
		, newOrders :: Set [ no :: NewOrder | no :: Id <- matches (GL "*") ]
		, orders :: Map [ o :: Order | o :: Id <- matches (GL "*") ]
	}
	
	type Customer = File CustomerInfo
	
	type NewOrder = SymLink where (foreignKey this_att "../Orders")
	
	type Order = Directory {
		  orderCustomer matches (RE "*.Customer") :: SymLink where (foreignKey orderCustomer_att "../../Customers")
		, orderLines :: Map [ l :: OrderLine | l :: Id <- matches (GL "*") ]
		, orderInfo :: File (OrderInfo <| fromIntegral (Map.size $ snd orderLines) |>)
	}
	
	type OrderLine = Directory {
		  orderLineItem matches (RE "*.Item") :: SymLink where (foreignKey orderLineItem_att "../../../Stock")
		, orderLineSupplyWarehouse matches (RE "*.Warehouse") :: SymLink where (foreignKey orderLineSupplyWarehouse_att "../../../../Warehouses")
		, orderLineInfo :: File OrderLineInfo
	}
	
	type StockItem = File StockItemInfo
	
|]

foreignKey :: FileInfo -> FilePath -> Bool
foreignKey info path = Just (path </> takeFileName (fullpath info)) == symLink info

-- One non-repeating group of fields: W_ID, D_ID, C_ID, O_ID, O_OL_CNT, C_LAST, C_CREDIT,
-- C_DISCOUNT, W_TAX, D_TAX, O_ENTRY_D, total_amount, and an optional execution status message other
-- than "Item number is not valid".
type NewOrderOutput = (Id,Id,Id,Id,Unsigned_2,VText,FText,Signed_4_4,Signed_4_4,Signed_4_4,DateTime,Signed_6_2,[NewOrderOutputLine])
-- One repeating group of fields: OL_SUPPLY_W_ID, OL_I_ID, I_NAME, OL_QUANTITY, S_QUANTITY,
-- brand_generic, I_PRICE, and OL_AMOUNT. The group is repeated O_OL_CNT times (once per item in the
-- order), equal to the computed value of ol_cnt.
type NewOrderOutputLine = (Id,Id,VText,Unsigned_2,Signed_4,String,Signed_5_2,Signed_6_2)

eth (x1,x2,x3,x4,x5,x6,x7,x8) = x8

-- For a given warehouse number (W_ID), district number (D_W_ID , D_ID), customer number (C_W_ID
-- , C_D_ID , C_ ID), count of items (ol_cnt, not communicated to the SUT), and for a given set of items (OL_I_ID),
-- supplying warehouses (OL_SUPPLY_W_ID), and quantities (OL_QUANTITY):
newOrder :: Id -> Id -> Id -> Map Id (Id,Unsigned_2) -> IO NewOrderOutput
newOrder w_id d_id c_id order = atomically $ do
	
	-- The row in the WAREHOUSE table with matching W_ID is selected and W_TAX, the warehouse tax rate, is
	-- retrieved.
	
	co_var :: Company TxVarFS <- new () "company"
	(co_md,co) <- read co_var
	let (ws_md,ws) = warehouses co
	w_var :: Warehouse TxVarFS <- lookupEntry w_id ws
	(w_md,w) <- read w_var
	let (w_info_md,w_info) = warehouseInfo w
	let w_tax = warehouseTax w_info
	
	-- The row in the DISTRICT table with matching D_W_ID and D_ID is selected, D_TAX, the district tax rate, is
	-- retrieved, and D_NEXT_O_ID, the next available order number for the district, is retrieved and incremented
	-- by one.
	
	let (ds_md,ds) = districts w
	d_var :: District TxVarFS <- lookupEntry d_id ds
	(d_md,d) <- read d_var
	let (d_info_md,d_info) = districtInfo d
	let d_tax = districtTax d_info
	let d_next_o = succ $ districtNextOrder d_info 
	writeOrThrow d_var (d_md,d { districtInfo = (d_info_md,d_info { districtNextOrder = d_next_o }) }) WriteFailed
	
	-- The row in the CUSTOMER table with matching C_W_ID, C_D_ID, and C_ID is selected and C_DISCOUNT,
	-- the customer's discount rate, C_LAST, the customer's last name, and C_CREDIT, the customer's credit status,
	-- are retrieved.
	
	let (cs_md,cs) = customers d
	c_var :: Customer TxVarFS <- lookupEntry c_id cs
	(c_md,c)  <- read c_var
	let c_discount = customerDiscount c
	let c_last = lastName $ customerName c
	let c_credit = customerCredit c
	
	-- A new row is inserted into both the NEW-ORDER table and the ORDER table to reflect the creation of the
	-- new order. O_CARRIER_ID is set to a null value. If the order includes only home order-lines, then
	-- O_ALL_LOCAL is set to 1, otherwise O_ALL_LOCAL is set to 0.
	
	let (os_md,os) = orders d
	let o_path = fullpath os_md </> printRep d_next_o
	o_var :: Order TxVarFS <- new () o_path
	let o_c_path = o_path </> addExtension (printRep c_id) "Customer"
	let o_c_path_tgt = o_c_path </> "../../Customers" </> (printRep c_id)
	let o_c = ((cleanSymLinkFileInfo o_c_path o_c_path_tgt,cleanBasePD),o_c_path_tgt)
	let o_local = and $ map ((==w_id) . fst) $ Map.elems order
	let o_cnt = toEnum $ Map.size order
	current_time <- unsafeIOToFTM getCurrentTime
	let o_info = OrderInfo current_time Nothing o_cnt o_local
	let o_info_md = (cleanFileInfo $ o_path </> "orderInfo",Pads.defaultMd1 o_cnt o_info)
	let o_ls = (cleanFileInfo $ o_path </> "orderLines",Map.empty)
	let o = Order_inner o_c o_ls (o_info_md,o_info)
	writeOrThrow o_var (cleanFileInfo o_path,o) WriteFailed
	
	-- For each O_OL_CNT item on the order:
	
	let newOrderItem (ol_ix,(ol_i_id,(ol_supply_w_id,ol_quantity))) = do
		
		-- The row in the ITEM table with matching I_ID (equals OL_I_ID) is selected and I_PRICE, the price of the
		-- item, I_NAME, the name of the item, and I_DATA are retrieved.
	
		let (is_md,is) = items co
		i_var :: Item TxVarFS <- lookupEntry ol_i_id is
		(i_md,i) <- read i_var
		let i_price = itemPrice i
		let i_name = itemName i
		let i_data = itemData i
		
		-- The row in the STOCK table with matching S_I_ID (equals OL_I_ID) and S_W_ID (equals
		-- OL_SUPPLY_W_ID) is selected. S_QUANTITY, the quantity in stock, S_DIST_xx, where xx represents the
		-- district number, and S_DATA are retrieved. If the retrieved value for S_QUANTITY exceeds
		-- OL_QUANTITY by 10 or more, then S_QUANTITY is decreased by OL_QUANTITY; otherwise
		-- S_QUANTITY is updated to (S_QUANTITY - OL_QUANTITY)+91. S_YTD is increased by
		-- OL_QUANTITY and S_ORDER_CNT is incremented by 1. If the order-line is remote, then
		-- S_REMOTE_CNT is incremented by 1.
		
		ol_supply_w_var :: Warehouse TxVarFS <- lookupEntry ol_supply_w_id ws
		(ol_supply_w_md,ol_supply_w) <- read ol_supply_w_var
		let (s_md,s) = stock ol_supply_w
		s_i_var :: StockItem TxVarFS <- lookupEntry ol_i_id s
		(s_i_md,s_i) <- read s_i_var
		let s_i_quantity = stockItemQuantity s_i
		let s_i_dists = stockItemDists s_i
		let s_i_data = stockItemData s_i
		
		let s_i_quantity' = s_i_quantity - fromIntegral ol_quantity
		let (s_i_quantity'',s_i') = if (s_i_quantity' > 10)
			then (s_i_quantity',s_i { stockItemQuantity = s_i_quantity' })
			else
				let s_i_quantity'' = s_i_quantity' + 91
				in (s_i_quantity'',s_i { stockItemQuantity = s_i_quantity'', stockItemYTD = stockItemYTD s_i + fromIntegral ol_quantity, stockItemOrderCount = succ (stockItemOrderCount s_i) })
		let s_i'' = if (ol_supply_w_id == w_id) then s_i' else s_i' { stockItemRemoteCount = succ (stockItemRemoteCount s_i') }
		writeOrThrow s_i_var (s_i_md,s_i'') WriteFailed
	
		-- The amount for the item in the order (OL_AMOUNT) is computed as: OL_QUANTITY * I_PRICE
		
		let ol_amount = fromIntegral ol_quantity * i_price
		
		-- The strings in I_DATA and S_DATA are examined. If they both include the string "ORIGINAL", the brand-generic
		-- field for that item is set to "B", otherwise, the brand-generic field is set to "G".
		
		let brand_generic = if (i_data == "ORIGINAL" && s_i_data == "ORIGINAL") then "B" else "G"
	
		-- A new row is inserted into the ORDER-LINE table to reflect the item on the order. OL_DELIVERY_D is set
		-- to a null value, OL_NUMBER is set to a unique value within all the ORDER-LINE rows that have the same
		-- OL_O_ID value, and OL_DIST_INFO is set to the content of S_DIST_xx, where xx represents the district
		-- number (OL_D_ID)
	
		let ol_path = o_path </> "orderLines" </> printRep ol_i_id
		ol_var :: OrderLine TxVarFS <- new () ol_path
		let ol_md = cleanFileInfo ol_path
		let ol_i_path = ol_path </> addExtension (printRep ol_i_id) "Item"
		let ol_i_path_tgt = ol_i_path </> "../../../Stock" </> (printRep ol_i_id)
		let ol_i = ((cleanSymLinkFileInfo ol_i_path ol_i_path_tgt,cleanBasePD),ol_i_path_tgt)
		let ol_supply_w_path = ol_path </> addExtension (printRep ol_supply_w_id) "Warehouse"
		let ol_supply_w_path_tgt = ol_i_path </> "../../../../Warehouses" </> (printRep ol_supply_w_id)
		let ol_supply_w = ((cleanSymLinkFileInfo ol_supply_w_path ol_supply_w_path_tgt,cleanBasePD),ol_supply_w_path_tgt)
		let ol_info = OrderLineInfo ol_ix Nothing ol_quantity ol_amount (s_i_dists !! succ (fromEnum d_id))
		let ol_info_md = (cleanFileInfo $ ol_path </> "orderLineInfo",Pads.defaultMd ol_info)
		let ol = OrderLine_inner ol_i ol_supply_w (ol_info_md,ol_info)
		writeOrThrow ol_var (ol_md,ol) WriteFailed
	
		return (ol_supply_w_id,ol_i_id,i_name,ol_quantity,s_i_quantity'',brand_generic,i_price,ol_amount)
	
	ols <- mapM newOrderItem $ zip [1..] $ Map.toList order
	let sum_ol_amount = sum $ map eth ols
	
	-- The total-amount for the complete order is computed as: sum(OL_AMOUNT) * (1 - C_DISCOUNT) * (1 + W_TAX + D_TAX)
	
	let total_amount = sum_ol_amount * (1 - c_discount) * (1 + w_tax + d_tax)
	
	return (w_id,d_id,c_id,d_next_o,o_cnt,c_last,c_credit,c_discount,w_tax,d_tax,current_time,total_amount,ols)

-- The following fields are displayed: W_ID,
-- D_ID, C_ID, C_D_ID, C_W_ID, W_LOCATION, D_LOCATION, C_PERSON, C_LOCATION, C_PHONE, C_SINCE, C_CREDIT, C_CREDIT_LIM, C_DISCOUNT, C_BALANCE, the first 200
-- characters of C_DATA (only if C_CREDIT = "BC"), H_AMOUNT, and H_DATE.
type PaymentOutput = (Id,Id,Id,Id,Location,Location,Person,Location,FText,DateTime,FText,Signed_12_2,Signed_4_4,Signed_12_2,Maybe VText,Signed_6_2,DateTime)

-- For a given warehouse number (W_ID), district number (D_W_ID , D_ID), customer number (C_W_ID
-- , C_D_ID , C_ ID) or customer last name (C_W_ID , C_D_ID , C_LAST), and payment amount (H_AMOUNT):
payment :: Id -> Id -> (Id,Id,Either Id VText) -> Signed_6_2 -> IO PaymentOutput
payment w_id d_id (c_w_id,c_d_id,c_id_or_last) h_amount = atomically $ do
	
	-- The row in the WAREHOUSE table with matching W_ID is selected. W_NAME, W_STREET_1,
	-- W_STREET_2, W_CITY, W_STATE, and W_ZIP are retrieved and W_YTD, the warehouse's year-to-date
	-- balance, is increased by H_ AMOUNT.
	
	co_var :: Company TxVarFS <- new () "company"
	(co_md,co) <- read co_var
	let (ws_md,ws) = warehouses co
	w_var :: Warehouse TxVarFS <- lookupEntry w_id ws
	(w_md,w) <- read w_var
	let (w_info_md,w_info) = warehouseInfo w
	let w_name = warehouseName w_info
	let w_loc = warehouseLocation w_info
	let w_street = locationStreet w_loc
	let w_city = locationCity w_loc
	let w_state = locationState w_loc
	let w_zip = locationZip w_loc
	let w_ytd' = warehouseYTD w_info + realToFrac h_amount
	writeOrThrow w_var (w_md,w { warehouseInfo = (w_info_md,w_info { warehouseYTD = w_ytd' }) }) WriteFailed
	
	-- The row in the DISTRICT table with matching D_W_ID and D_ID is selected. D_NAME, D_STREET_1,
	-- D_STREET_2, D_CITY, D_STATE, and D_ZIP are retrieved and D_YTD, the district's year-to-date balance, is
	-- increased by H_AMOUNT.
	
	let (ds_md,ds) = districts w
	d_var :: District TxVarFS <- lookupEntry d_id ds
	(d_md,d) <- read d_var
	let (d_info_md,d_info) = districtInfo d
	let d_name = districtName d_info
	let d_loc = districtLocation d_info
	let d_street = locationStreet d_loc
	let d_city = locationCity d_loc
	let d_state = locationState d_loc
	let d_zip = locationZip d_loc
	let d_ytd' = districtYTD d_info + realToFrac h_amount
	writeOrThrow d_var (d_md,d { districtInfo = (d_info_md,d_info { districtYTD = d_ytd' }) }) WriteFailed
	
	c_w_var <- lookupEntry c_w_id ws
	(c_w_md,c_w) <- read c_w_var
	let (c_ds_md,c_ds) = districts c_w
	c_d_var <- lookupEntry c_d_id c_ds
	(c_d_md,c_d) <- read c_d_var
	let (cs_md,cs) = customers c_d
	(c_id,c_var,(c_md,c)) <- case c_id_or_last of
	
		-- Case 1, the customer is selected based on customer number: the row in the CUSTOMER table with matching
		-- C_W_ID, C_D_ID and C_ID is selected.
	
		Left c_id -> do
			c_var <- lookupEntry c_id cs
			(c_md,c) <- read c_var
			return (c_id,c_var,(c_md,c))
			
		-- Case 2, the customer is selected based on customer last name: all rows in the CUSTOMER table with
		-- matching C_W_ID, C_D_ID and C_LAST are selected sorted by C_FIRST in ascending order. Let n be the
		-- number of rows selected. Select the row at position (n/ 2 rounded up to the next integer) in the sorted set of selected rows from the
		-- CUSTOMER table.
		
		Right c_last -> do
			let sorted_filter_c_last c_id c_var mxs = do
				(c_md,c) <- read c_var
				let c_name = customerName c
				if (c_last == lastName c_name)
					then liftM (List.insert (firstName c_name,(c_id,c_var,(c_md,c)))) mxs
					else mxs
			sorted_cs <- Map.foldrWithKey sorted_filter_c_last (return []) cs
			return $ snd $ sorted_cs !! succ (ceiling $ toEnum (length sorted_cs) / 2)
		
	-- C_ID, C_FIRST, C_MIDDLE, C_STREET_1, C_STREET_2, C_CITY, C_STATE,
	-- C_ZIP, C_PHONE, C_SINCE, C_CREDIT, C_CREDIT_LIM, C_DISCOUNT, and C_BALANCE are retrieved	
	-- C_BALANCE is decreased by H_AMOUNT. C_YTD_PAYMENT is increased by H_AMOUNT.
	-- C_PAYMENT_CNT is incremented by 1.
	let c_person = customerName c
	let c_first = firstName c_person
	let c_middle = middleName c_person
	let c_loc = customerLocation c
	let c_street = locationStreet c_loc
	let c_city = locationCity c_loc
	let c_state = locationState c_loc
	let c_zip = locationZip c_loc
	let c_phone = customerPhone c
	let c_since = customerSince c
	let c_credit = customerCredit c
	let c_credit_lim = customerCreditLimit c
	let c_discount = customerDiscount c
	let c_balance' = customerBalance c - realToFrac h_amount
	let c_ytd_payment' = customerYTDPayment c + realToFrac h_amount
	
	-- If the value of C_CREDIT is equal to "BC", then C_DATA is also retrieved from the selected customer and the
	-- following history information: C_ID, C_D_ID, C_W_ID, D_ID, W_ID, and H_AMOUNT, are inserted at the
	-- left of the C_DATA field by shifting the existing content of C_DATA to the right by an equal number of bytes
	-- and by discarding the bytes that are shifted out of the right side of the C_DATA field. The content of the
	-- C_DATA field never exceeds 500 characters. The selected customer is updated with the new C_DATA field.
	
	let c_data = customerData c
	let c_data' = if (c_credit == "BC")
		then take 500 $ printRep c_id ++ printRep c_d_id ++ printRep c_w_id ++ printRep d_id ++ printRep w_id ++ printRep h_amount ++ c_data
		else c_data
	
	writeOrThrow c_var (c_md,c { customerBalance = c_balance', customerYTDPayment = c_ytd_payment', customerData = c_data' }) WriteFailed
	
	-- H_DATA is built by concatenating W_NAME and D_NAME separated by 4 spaces.
	
	let h_data = w_name ++ "    " ++ d_name
	
	-- A new row is inserted into the HISTORY table with H_C_ID = C_ID, H_C_D_ID = C_D_ID, H_C_W_ID =
	-- C_W_ID, H_D_ID = D_ID, and H_W_ID = W_ID.
	
	let h_var = history co
	(h_md,HistoryTable h) <- read h_var
	h_date <- unsafeIOToFTM getCurrentTime
	let h_entry = HistoryEntry c_id c_d_id c_w_id d_id w_id h_date h_amount h_data
	writeOrThrow h_var (h_md,HistoryTable $ h_entry:h) WriteFailed
	
	let ret_c_data = if (c_credit == "BC") then Just (take 200 c_data') else Nothing
	return (d_id,c_id,c_d_id,c_w_id,w_loc,d_loc,c_person,c_loc,c_phone,c_since,c_credit,c_credit_lim,c_discount,c_balance',ret_c_data,h_amount,h_date)
	
-- One non-repeating group of fields: W_ID, D_ID, C_ID, C_Person, C_BALANCE, O_ID,
-- O_ENTRY_D, and O_CARRIER_ID; 
type OrderStatusOutput = (Id,Id,Id,Person,Signed_12_2,Id,DateTime,Maybe Id,[OrderStatusOutputLine])
--One repeating group of fields: OL_SUPPLY_W_ID, OL_I_ID, OL_QUANTITY, OL_AMOUNT, and
-- OL_DELIVERY_D. The group is repeated O_OL_CNT times (once per item in the order).
type OrderStatusOutputLine = (Id,Id,Unsigned_2,Signed_6_2,Maybe DateTime)

-- For a given customer number (C_W_ID , C_D_ID , C_ ID):
orderStatus :: Id -> Id -> Either Id VText -> IO OrderStatusOutput
orderStatus w_id d_id c_id_or_last = atomically $ do
	
	co_var :: Company TxVarFS <- new () "company"
	(co_md,co) <- read co_var
	let (ws_md,ws) = warehouses co
	w_var :: Warehouse TxVarFS <- lookupEntry w_id ws
	(w_md,w) <- read w_var
	let (ds_md,ds) = districts w
	d_var :: District TxVarFS <- lookupEntry d_id ds
	(d_md,d) <- read d_var
	let (cs_md,cs) = customers d
	
	(c_id,c_var,(c_md,c)) <- case c_id_or_last of
	
		-- Case 1, the customer is selected based on customer number: the row in the CUSTOMER table with matching
		-- C_W_ID, C_D_ID, and C_ID is selected.
		
		Left c_id -> do
				c_var <- lookupEntry c_id cs
				(c_md,c) <- read c_var
				return (c_id,c_var,(c_md,c))
			
		-- Case 2, the customer is selected based on customer last name: all rows in the CUSTOMER table with
		-- matching C_W_ID, C_D_ID and C_LAST are selected sorted by C_FIRST in ascending order. Let n be the
		-- number of rows selected. from the row at position n/ 2 rounded up in the sorted set of selected rows from the CUSTOMER table.
	
		Right c_last -> do
			let sorted_filter_c_last c_id c_var mxs = do
				(c_md,c) <- read c_var
				let c_name = customerName c
				if (c_last == lastName c_name)
					then liftM (List.insert (firstName c_name,(c_id,c_var,(c_md,c)))) mxs
					else mxs
			sorted_cs <- Map.foldrWithKey sorted_filter_c_last (return []) cs
			return $ snd $ sorted_cs !! succ (ceiling $ toEnum (length sorted_cs) / 2)
	
	-- C_BALANCE, C_FIRST, C_MIDDLE, and C_LAST are retrieved.
	let c_balance = customerBalance c
	let c_person = customerName c
	let c_first = firstName c_person
	let c_middle = middleName c_person
	let c_last = middleName c_person
	
	-- The row in the ORDER table with matching O_W_ID (equals C_W_ID), O_D_ID (equals C_D_ID), O_C_ID
	-- (equals C_ID), and with the largest existing O_ID, is selected. This is the most recent order placed by that
	-- customer. O_ID, O_ENTRY_D, and O_CARRIER_ID are retrieved.
	
	let (os_md,os) = orders d
	let (o_id,o_var) = Map.findMax os
	(o_md,o) <- read o_var
	let (o_info_md,o_info) = orderInfo o
	let o_entry_d = orderEntryDate o_info
	let o_carrier_id = orderCarrier o_info
	
	-- All rows in the ORDER-LINE table with matching OL_W_ID (equals O_W_ID), OL_D_ID (equals O_D_ID),
	-- and OL_O_ID (equals O_ID) are selected and the corresponding sets of OL_I_ID, OL_SUPPLY_W_ID,
	-- OL_QUANTITY, OL_AMOUNT, and OL_DELIVERY_D are retrieved.
	
	let (ols_md,ols) = orderLines o
	let viewOrderLine ol_var = do
		(ol_md,ol) <- read ol_var
		let (ol_supply_w_md,ol_supply_w_tgt) = orderLineSupplyWarehouse ol
		let ol_supply_w_id = parseRep $ takeFileName ol_supply_w_tgt
		let (ol_i_md,ol_i_tgt) = orderLineItem ol
		let ol_i_id = parseRep $ takeFileName ol_i_tgt
		let (ol_info_md,ol_info) = orderLineInfo ol
		let ol_quantity = orderLineQuantity ol_info
		let ol_amount = orderLineAmount ol_info
		let ol_delivery_d = orderLineDeliveryDate ol_info
		return (ol_supply_w_id,ol_i_id,ol_quantity,ol_amount,ol_delivery_d)
	
	ols_ret <- mapM viewOrderLine $ Map.elems ols
	
	return (w_id,d_id,c_id,c_person,c_balance,o_id,o_entry_d,o_carrier_id,ols_ret)
	
-- For a given warehouse number (W_ID) and for a given carrier number (O_CARRIER_ID):
delivery :: Id -> Id -> IO ()
delivery w_id o_carrier_id = atomically $ do
	
	co_var :: Company TxVarFS <- new () "company"
	(co_md,co) <- read co_var
	let (ws_md,ws) = warehouses co
	w_var :: Warehouse TxVarFS <- lookupEntry w_id ws
	(w_md,w) <- read w_var
	let (ds_md,ds) = districts w
	
	-- For each of the 10 districts (D_W_ID , D_ID) within that warehouse
	let deliveryDistrict d_var = do
		(d_md,d) <- read d_var
		let (nos_md,nos) = newOrders d
	
		-- The row in the NEW-ORDER table with matching NO_W_ID (equals W_ID) and NO_D_ID (equals D_ID)
		-- and with the lowest NO_O_ID value is selected. This is the oldest undelivered order of that district.
		-- NO_O_ID, the order number, is retrieved. If no matching row is found, then the delivery of an order for this
		-- district is skipped.
	
		case Set.minView nos of
			Nothing -> return ()
			Just ((no_id,no_var),nos') -> do
				
				-- The selected row in the NEW-ORDER table is deleted.
				writeOrThrow d_var (d_md,d { newOrders = (nos_md,nos') }) WriteFailed
				
				-- The row in the ORDER table with matching O_W_ID (equals W_ ID), O_D_ID (equals D_ID), and O_ID
				-- (equals NO_O_ID) is selected, O_C_ID, the customer number, is retrieved, and O_CARRIER_ID is updated.
				let (os_md,os) = orders d
				o_var <- lookupEntry no_id os
				(o_md,o) <- read o_var
				let (o_c_md,o_c_tgt) = orderCustomer o
				let o_c_id = parseRep $ takeFileName o_c_tgt
				let (o_info_md,o_info) = orderInfo o
				let o_info' = o_info { orderCarrier = Just o_carrier_id }
				let o' = o { orderInfo = (o_info_md,o_info')  }
				writeOrThrow o_var (o_md,o') WriteFailed
	
				-- All rows in the ORDER-LINE table with matching OL_W_ID (equals O_W_ID), OL_D_ID (equals O_D_ID),
				-- and OL_O_ID (equals O_ID) are selected. All OL_DELIVERY_D, the delivery dates, are updated to the
				-- current system time as returned by the operating system and the sum of all OL_AMOUNT is retrieved.
				let (ols_md,ols) = orderLines o
				current_time <- unsafeIOToFTM getCurrentTime
				let updateOrderLine acc ol_var = do
					(ol_md,ol) <- read ol_var
					let (ol_info_md,ol_info) = orderLineInfo ol
					let ol_amount = orderLineAmount ol_info
					let ol_info' = ol_info { orderLineDeliveryDate = Just current_time }
					let ol' = ol { orderLineInfo = (ol_info_md,ol_info') }
					writeOrThrow ol_var (ol_md,ol') WriteFailed
					return $ acc + ol_amount
				ols_amount <- Foldable.foldlM updateOrderLine 0 ols
				
				-- The row in the CUSTOMER table with matching C_W_ID (equals W_ID), C_D_ID (equals D_ID), and C_ID
				-- (equals O_C_ID) is selected and C_BALANCE is increased by the sum of all order-line amounts
				-- (OL_AMOUNT) previously retrieved. C_DELIVERY_CNT is incremented by 1.
				let (cs_md,cs) = customers d
				c_var :: Customer TxVarFS <- lookupEntry o_c_id cs
				(c_md,c) <- read c_var
				let c_balance = customerBalance c
				let c_delivery_cnt = customerDeliveryCount c
				let c' = c { customerBalance = c_balance + realToFrac ols_amount, customerDeliveryCount = succ c_delivery_cnt }
				writeOrThrow c_var (c_md,c') WriteFailed
				
	Foldable.mapM_ deliveryDistrict ds

--	The following fields are displayed: W_ID, D_ID, threshold, and low_stock.
type StockLevelOutput = (Id,Id,Signed_4,[(Id,StockItemInfo)])

-- 	For a given warehouse number (W_ID), district num ber (D_W_ID , D_ID), and stock level threshold:
stockLevel :: Id -> Id -> Signed_4 -> IO StockLevelOutput
stockLevel w_id d_id threshold = atomically $ do
	
	-- The row in the DISTRICT table with matching D_W_ID and D_ID is selected and D_NEXT_O_ID is retrieved. 
	co_var :: Company TxVarFS <- new () "company"
	(co_md,co) <- read co_var
	let (ws_md,ws) = warehouses co
	w_var <- lookupEntry w_id ws
	(w_md,w) <- read w_var
	let (ds_md,ds) = districts w
	d_var <- lookupEntry d_id ds
	(d_md,d) <- read d_var
	let (d_info_md,d_info) = districtInfo d
	let d_next_o_id = districtNextOrder d_info
	
	-- All rows in the ORDER-LINE table with matching OL_W_ID (equals W_ID), OL_D_ID (equals D_ID), and
	-- OL_O_ID (lower than D_NEXT_O_ID and greater than or equal to D_NEXT_O_ID minus 20) are selected.
	-- They are the items for 20 recent orders of the district. 
	
	let (os_md,os) = orders d
	let os_20 = Map.filterWithKey (\o_id o_var -> (d_next_o_id - 20) <= o_id && o_id < d_next_o_id) os
	let getOrderLineItems is ol_var = do
		(ol_md,ol) <- read ol_var
		let (ol_i_md,ol_i_tgt) = orderLineItem ol
		let ol_i = parseRep $ takeFileName ol_i_tgt
		return $ Set.insert ol_i is
	let getOrderItems is o_var = do
		(o_md,o) <- read o_var
		let (ols_md,ols) = orderLines o
		Foldable.foldlM getOrderLineItems is ols
	is <- Foldable.foldlM getOrderItems Set.empty os_20
	
	-- All rows in the STOCK table with matching S_I_ID (equals OL_I_ID) and S_W_ID (equals W_ID) from the list
	-- of distinct item numbers and with S_QUANTITY lower than threshold are counted (giving low_stock).
	
	let (s_md,s) = stock w
	let getItemStock s_id s_var mxs = do
		if s_id `Set.member` is
			then do
				(s_md,s) <- read s_var
				let s_quantity = stockItemQuantity s
				if s_quantity < threshold
					then liftM ((s_id,s):) mxs
					else mxs
			else mxs
	low_is <- Map.foldrWithKey getItemStock (return []) s
	return (w_id,d_id,threshold,low_is)

lookupEntry :: Ord k => k -> Map k v -> FTM TxVarFS v
lookupEntry k m = case Map.lookup k m of
	Nothing -> throw EntryNotFound
	Just v -> return v

data TPCCException = forall e . Exception e => TPCCException e
    deriving Typeable

instance Show TPCCException where
	show (TPCCException e) = show e
	
instance Exception TPCCException

tpccExceptionToException :: Exception e => e -> SomeException
tpccExceptionToException = toException . TPCCException

tpccExceptionFromException :: Exception e => SomeException -> Maybe e
tpccExceptionFromException x = do
    TPCCException a <- fromException x
    cast a

data EntryNotFound = EntryNotFound deriving (Show,Typeable)

instance Exception EntryNotFound where
	toException = tpccExceptionToException
	fromException = tpccExceptionFromException

data WriteFailed = WriteFailed deriving (Show,Typeable)

instance Exception WriteFailed where
	toException = tpccExceptionToException
	fromException = tpccExceptionFromException


