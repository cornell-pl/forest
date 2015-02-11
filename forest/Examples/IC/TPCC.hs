{-# LANGUAGE ExistentialQuantification, RankNTypes, TupleSections, OverlappingInstances, TypeFamilies, StandaloneDeriving, TypeOperators, ConstraintKinds, DataKinds, UndecidableInstances, FlexibleContexts, TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, ViewPatterns #-}

-- The TPC-C transaction benchmark as found in http://www.tpc.org/tpc_documents_current_versions/pdf/tpc-c_v5-11.pdf

module Examples.IC.TPCC where

import System.Random.Shuffle
import System.Posix.Files
import Data.Hashable
import Data.Bits
import Prelude hiding (mod,read,const)
import qualified Prelude
import Control.Monad.Incremental.Adapton
import Control.Monad.Lazy
import Control.Monad.Incremental.Display
import Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.Reader as Reader
import Data.List
import Data.Time
import qualified Data.Text as Text
import Data.Char
import Data.Maybe
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Test.QuickCheck.Arbitrary
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
	
	data OrderInfo = OrderInfo {
		  orderEntryDate :: Line DateTime
		, orderCarrier :: Line (Maybe Id)
		, orderLinesCount :: Line Unsigned_2
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
	
	data HistoryEntryInfo = HistoryEntryInfo {
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
		  warehouses :: Warehouses
		, items :: Items
		, history :: History
	} where <| liftM and $ sequence [consistentWarehouseHistory this,consistentDistrictHistory this,consistentBalance this] |>
	
	type Warehouses = Map [ w :: Warehouse | w :: Id <- matches (GL "*") ]
	type Items = Map [ i :: Item | i :: Id <- matches (GL "*") ]
	
	type History = [ h :: HistoryEntry | h <- matches (GL "*") ]
	type HistoryEntry = File HistoryEntryInfo

	type Warehouse = Directory {
		  warehouseInfo :: File WarehouseInfo
		, districts :: Districts
		, stock :: Stock
	} where <| consistentYTD this |>
	
	type Districts = Map [ d :: District | d :: Id <- matches (GL "*") ]
	type Stock = Map [ s :: StockItem | s :: Id <- matches (GL "*") ]
	
	type Item = File ItemInfo
	
	type District = Directory {
		  districtInfo :: File DistrictInfo
		, customers :: Customers
		, newOrders :: NewOrders
		, orders :: Orders
	} where <| liftM and $ sequence [consistentNextOrder this,consistentOrdersNewOrders this,consistentCustomerBalance this] |>
	
	type Customers = Map [ c :: Customer | c :: Id <- matches (GL "*") ]
	type NewOrders = Set [ no :: NewOrder | no :: Id <- matches (GL "*") ] where <| consistentNewOrders this |>
	type Orders = Map [ o :: Order | o :: Id <- matches (GL "*") ]
	
	type Customer = File CustomerInfo
	
	type NewOrder = SymLink where (foreignKey this_att "../Orders")
	
	type Order = Directory {
		  orderCustomer matches (RE "*.Customer") :: SymLink where (foreignKey orderCustomer_att "../../Customers")
		, orderLines :: OrderLines
		, orderInfo :: File OrderInfo
	} where <| liftM and $ sequence [consistentOrderLinesCount this,consistentOrderDelivery this] |>
	
	type OrderLines = Map [ l :: OrderLine | l :: Id <- matches (GL "*") ]
	
	type OrderLine = Directory {
		  orderLineItem matches (RE "*.Item") :: SymLink where (foreignKey orderLineItem_att "../../../Stock")
		, orderLineSupplyWarehouse matches (RE "*.Warehouse") :: SymLink where (foreignKey orderLineSupplyWarehouse_att "../../../../Warehouses")
		, orderLineInfo :: File OrderLineInfo
	}
	
	type StockItem = File StockItemInfo
	
|]

-- * Consistency conditions

--1) Entries in the WAREHOUSE and DISTRICT tables must satisfy the relationship: W_YTD = sum(D_YTD)
consistentYTD (w_md,w) = do
	let (w_info_md,w_info) = warehouseInfo w
	(ds_md,ds) <- read (districts w)
	let w_ytd = warehouseYTD w_info
	let sumYTDs acc d_var = do
		(d_md,d) <- read d_var
		let (d_info_md,d_info) = districtInfo d
		return $ acc + districtYTD d_info
	ds_ytd <- Foldable.foldlM sumYTDs 0 ds
	return $ w_ytd == ds_ytd

-- 2) Entries in the DISTRICT, ORDER, and NEW-ORDER tables must satisfy the relationship:
-- D_NEXT_O_ID - 1 = max(O_ID) = max(NO_O_ID)
-- for each district defined by (D_W_ID = O_W_ID = NO_W_ID) and (D_ID = O_D_ID = NO_D_ID). This condition
-- does not apply to the NEW-ORDER table for any districts which have no outstanding new orders (i.e., the number of rows is zero).
consistentNextOrder (d_md,d) = do
	let (d_info_md,d_info) = districtInfo d
	let d_next_o_id = districtNextOrder d_info
	(os_md,os) <- read (orders d)
	let os_b = case Map.maxViewWithKey os of
		Nothing -> d_next_o_id - 1 == 0
		Just ((o_id,_),_) -> d_next_o_id - 1 == o_id
	(nos_md,nos) <- read (newOrders d)
	let nos_b = case Map.maxViewWithKey os of
		Nothing -> True
		Just ((no_id,_),_) -> d_next_o_id - 1 == no_id
	return $ os_b && nos_b

-- 3) Entries in the NEW-ORDER table must satisfy the relationship:
-- max(NO_O_ID) - min(NO_O_ID) + 1 = [number of rows in the NEW-ORDER table for this district]
-- for each district defined by NO_W_ID and NO_D_ID. This condition does not apply to any districts which have no
-- outstanding new orders (i.e., the number of rows is zero).
consistentNewOrders (nos_md,nos) = do
	let mb_max = Set.maxView nos
	let mb_min = Set.minView nos
	let nos_b = case (mb_max,mb_min) of
		(Just ((max_id,_),_),Just ((min_id,_),_)) -> max_id - min_id + 1 == fromIntegral (Set.size nos)
		otherwise -> True
	return nos_b

-- 5) For any row in the ORDER table, O_CARRIER_ID is set to a null value if and only if there is a corresponding row in
-- the NEW-ORDER table defined by (O_W_ID, O_D_ID, O_ID) = (NO_W_ID, NO_D_ID, NO_O_ID)
consistentOrdersNewOrders (d_md,d) = do
	(os_md,os) <- read (orders d)
	(nos_md,nos) <- read (newOrders d)
	let testOrder o_id o_var mb = do
		(o_md,o) <- read o_var
		let (o_info_md,o_info) = orderInfo o
		case (orderCarrier o_info,List.lookup o_id $ Set.toList nos) of
			(Nothing,Just _) -> mb
			(Just _,Nothing) -> mb
			otherwise -> return False
	Map.foldrWithKey testOrder (return True) os

-- 4) Entries in the ORDER and ORDER-LINE tables must satisfy the relationship:
-- sum(O_OL_CNT) = [number of rows in the ORDER-LINE table for this district]
-- for each district defined by (O_W_ID = OL_W_ID) and (O_D_ID = OL_D_ID).
-- 6) For any row in the ORDER table, O_OL_CNT must equal the number of rows in the ORDER-LINE table for the
-- corresponding order defined by (O_W_ID, O_D_ID, O_ID) = (OL_W_ID, OL_D_ID, OL_O_ID).
consistentOrderLinesCount (o_md,o) = do
	let (o_info_md,o_info) = orderInfo o
	let o_ol_cnt = orderLinesCount o_info
	(ols_md,ols) <- read (orderLines o)
	return $ o_ol_cnt == fromIntegral (Map.size ols)

-- 7) For any row in the ORDER-LINE table, OL_DELIVERY_D is set to a null date/ time if and only if the corresponding
-- row in the ORDER table defined by (O_W_ID, O_D_ID, O_ID) = (OL_W_ID, OL_D_ID, OL_O_ID) has
-- O_CARRIER_ID set to a null value.
consistentOrderDelivery (o_md,o) = do
	let (o_info_md,o_info) = orderInfo o
	(ols_md,ols) <- read (orderLines o)
	let o_carrier_id = orderCarrier o_info
	let testOL b ol_var = do
		(ol_md,ol) <- read ol_var
		let (ol_info_md,ol_info) = orderLineInfo ol
		let ol_delivery_d = orderLineDeliveryDate ol_info
		case (ol_delivery_d,o_carrier_id) of
			(Nothing,Nothing) -> return b
			(Just _,Just _) -> return b
			otherwise -> return False
	Foldable.foldlM testOL True ols

-- 8) Entries in the WAREHOUSE and HISTORY tables must satisfy the relationship:
-- W_YTD = sum(H_AMOUNT)
-- for each warehouse defined by (W_ID = H_W_ID).
consistentWarehouseHistory (co_md,co) = do
	(ws_md,ws) <- read (warehouses co)
	(h_md,h) <- read (history co)
	let testAmount w_id w_var mb = do
		(w_md,w) <- read w_var
		let (w_info_md,w_info) = warehouseInfo w
		let w_ytd = warehouseYTD w_info
		let sumH i (_,he_var) = do
			(he_md,he) <- read he_var
			return $ if (historyWarehouse he == w_id) then i + historyAmount he else i
		sum_h_w_id <- Foldable.foldlM sumH 0 h
		b <- mb
		return $ (w_ytd == realToFrac sum_h_w_id) && b
	Map.foldrWithKey testAmount (return True) ws

-- 9) Entries in the DISTRICT and HISTORY tables must satisfy the relationship:
-- D_YTD = sum(H_AMOUNT)
-- for each district defined by (D_W_ID, D_ID) = (H_W_ID, H_D_ID)
consistentDistrictHistory (co_md,co) = do
	(ws_md,ws) <- read (warehouses co)
	(h_md,h) <- read (history co)
	let testWAmount w_id w_var mb = do
		(w_md,w) <- read w_var
		(ds_md,ds) <- read (districts w)
		let testDAmount d_id d_var mb = do
			(d_md,d) <- read d_var
			let (d_info_md,d_info) = districtInfo d
			let d_ytd = districtYTD d_info
			let sumH i (_,he_var) = do
				(he_md,he) <- read he_var
				return $ if (historyDistrict he == d_id && historyWarehouse he == w_id) then i + historyAmount he else i
			sum_h_d_id <- Foldable.foldlM sumH 0 h
			b <- mb
			return $ (d_ytd == realToFrac sum_h_d_id) && b
		Map.foldrWithKey testDAmount mb ds
	Map.foldrWithKey testWAmount (return True) ws

-- 10) Entries in the CUSTOMER, HISTORY, ORDER, and ORDER-LINE tables must satisfy the relationship:
-- C_BALANCE = sum(OL_AMOUNT) - sum(H_AMOUNT)
-- where: H_AMOUNT is selected by (C_W_ID, C_D_ID, C_ID) = (H_C_W_ID, H_C_D_ID, H_C_ID)
-- and OL_AMOUNT is selected by:
-- (OL_W_ID, OL_D_ID, OL_O_ID) = (O_W_ID, O_D_ID, O_ID) and
-- (O_W_ID, O_D_ID, O_C_ID) = (C_W_ID, C_D_ID, C_ID) and
-- (OL_DELIVERY_D is not a null value)
consistentBalance (co_md,co) = do
	(ws_md,ws) <- read (warehouses co)
	(h_mh,h) <- read (history co)
	let testWarehouse w_id w_var mb = do
		(w_md,w) <- read w_var
		(ds_md,ds) <- read (districts w)
		let testDistrict d_id d_var mb = do
			(d_md,d) <- read d_var
			(cs_md,cs) <- read (customers d)
			(os_md,os) <- read (orders d)
			let testCustomer c_id c_var mb = do
				(c_md,c) <- read c_var
				let c_balance = customerBalance c
				let sumO i o_var = do
					(o_md,o) <- read o_var
					let (o_c_md,o_c_tgt) = orderCustomer o
					let o_c_id = parseRep $ takeFileName o_c_tgt
					if (c_id == o_c_id)
						then do
							(ols_md,ols) <- read (orderLines o)
							let sumOL i ol_var = do
								(ol_md,ol) <- read ol_var
								let (ol_info_md,ol_info) = orderLineInfo ol
								case orderLineDeliveryDate ol_info of
									Nothing -> return i
									Just utc -> return $ i + orderLineAmount ol_info
							Foldable.foldlM sumOL i ols
						else return i
				c_ol_amount <- Foldable.foldlM sumO 0 os
				let sumH i (_,he_var) = do
					(he_md,he) <- read he_var
					return $ if (historyCustomerWarehouse he == w_id && historyCustomerDistrict he == d_id && historyCustomer he == c_id) then i + historyAmount he else i
				c_h_amount <- Foldable.foldlM sumH 0 h
				b <- mb
				return $ (realToFrac c_balance == c_ol_amount - realToFrac c_h_amount) && b
			Map.foldrWithKey testCustomer mb cs
		Map.foldrWithKey testDistrict mb ds
	Map.foldrWithKey testWarehouse (return True) ws

-- 11) Entries in the CUSTOMER, ORDER and NEW-ORDER tables must satisfy the relationship:
-- (count(*) from ORDER) - (count(*) from NEW-ORDER) = 2100
-- for each district defined by (O_W_ID, O_D_ID) = (NO_W_ID, NO_D_ID) = (C_W_ID, C_D_ID).	

-- 12) Entries in the CUSTOMER and ORDER-LINE tables must satisfy the relationship:
-- C_BALANCE + C_YTD_PAYMENT = sum(OL_AMOUNT)
-- for any randomly selected customers and where OL_DELIVERY_D is not set to a null date/ time.
consistentCustomerBalance (d_md,d) = do
	(cs_md,cs) <- read (customers d)
	(os_md,os) <- read (orders d)
	let testCustomer c_id c_var mb = do
		(c_md,c) <- read c_var
		let c_balance = customerBalance c
		let c_ytd_payment = customerYTDPayment c
		let sumOrder i o_var = do
			(o_md,o) <- read o_var
			let (o_c_md,o_c_tgt) = orderCustomer o
			let o_c_id = parseRep $ takeFileName o_c_tgt
			if c_id == o_c_id
				then do
					(os_md,os) <- read (orderLines o)
					let sumOrderLine i ol_var = do
						(ol_md,ol) <- read ol_var
						let (ol_info_md,ol_info) = orderLineInfo ol
						let ol_amount = orderLineAmount ol_info
						let ol_delivery_d = orderLineDeliveryDate ol_info
						case ol_delivery_d of
							Nothing -> return i
							Just utc -> return $ i + ol_amount
					Foldable.foldlM sumOrderLine i os
				else return i
		sum_c_ols <- Foldable.foldlM sumOrder 0 os
		b <- mb
		return $ (c_balance + realToFrac c_ytd_payment == realToFrac sum_c_ols) && b
	Map.foldrWithKey testCustomer (return True) cs

foreignKey :: FileInfo -> FilePath -> Bool
foreignKey info path = Just (path </> takeFileName (fullpath info)) == symLink info

-- * Miscellaneous random data generating function

eth (x1,x2,x3,x4,x5,x6,x7,x8) = x8

c255 = unsafePerformIO $ generate $ choose (0,255)
c1023 = unsafePerformIO $ generate $ choose (0,1023)
c8191 = unsafePerformIO $ generate $ choose (0,8191)

-- c = random (0,a)
nuRand :: (Integer,Integer,Integer,Integer) -> Gen Integer
nuRand (a,c,x,y) = do
	i <- choose (0,a)
	j <- choose (x,y)
	return $ (((i .|. j) + c) `Prelude.mod` (y - x + 1)) + x

lastNameFromInt :: Int -> VText
lastNameFromInt n = (dict!!a ++ dict!!b ++ dict!!c) where
	dict = ["BAR","OUGHT","ABLE","PRI","PRES","ESE","ANTI","CALLY","ATION","EING"]
	a = div n 100
	b = div (Prelude.mod n 100) 10
	c = Prelude.mod n 10

a_string :: (Int,Int) -> IO String
a_string (x,y) = generate $ do
	l <- choose (x,y)
	vectorOf l arbitrary

n_string :: (Int,Int) -> IO String
n_string (x,y) = generate $ do
	l <- choose (x,y)
	vectorOf l $ choose ('0','9')

l_string :: (Int,Int) -> IO String
l_string = generate . l_string'
	
l_string' :: (Int,Int) -> Gen String
l_string' (x,y) = do
	l <- choose (x,y)
	vectorOf l $ oneof [choose ('a','z'),choose ('A','Z')]

rnd_zip :: IO String
rnd_zip = liftM (++ "11111") $ n_string (4,4)

rnd_last :: IO String
rnd_last = liftM lastNameFromInt $ generate $ choose (0,999)

populate :: Integer -> IO ()
populate w = do
	
	let h_path = root_db </> "history"
	createDirectoryIfMissing True h_path
	
	let ws_path = root_db </> "warehouses"
	createDirectoryIfMissing True ws_path
	let populateWarehouse w_id = do
		let w_path = ws_path </> show w_id
		let w_info_path = ws_path </> "warehouseInfo"
		createDirectoryIfMissing True w_info_path
		w_name <- a_string (6,10)
		w_street1 <- a_string (10,20)
		w_street2 <- a_string (10,20)
		w_city <- a_string (10,20)
		w_state <- l_string (2,2)
		w_zip <- rnd_zip
		w_tax <- generate $ choose (0,0.2)
		let w_ytd = 300000
		let w_info = WarehouseInfo w_name (Location [w_street1,w_street2] w_city w_state w_zip) w_tax w_ytd
		Pads.printFileRep w_info_path w_info
		
		let ds_path = ws_path </> "districts"
		createDirectoryIfMissing True ds_path
		let populateDistrict d_id = do
			let d_path = ds_path </> show d_id
			let d_info_path = d_path </> "districtInfo"
			d_name <- a_string (6,10)
			d_street1 <- a_string (10,20)
			d_street2 <- a_string (10,20)
			d_city <- a_string (10,20)
			d_state <- l_string (2,2)
			d_zip <- rnd_zip
			d_tax <- generate $ choose (0,0.2)
			Pads.printFileRep d_info_path $ DistrictInfo d_name (Location [d_street1,d_street2] d_city d_state d_zip) d_tax 30000 30001
			
			let cs_path = d_path </> "customers"
			createDirectoryIfMissing True cs_path
			let cs_ids = [1..3000]
			cs_oris <- generate $ vectorOf 300 $ elements cs_ids
			let populateCustomer c_id = do
				let c_path = cs_path </> show c_id
				c_first <- a_string (8,16)
				c_last <- rnd_last
				let c_name = Person c_first "OE" c_last
				c_street1 <- a_string (10,20)
				c_street2 <- a_string (10,20)
				c_city <- a_string (10,20)
				c_state <- l_string (2,2)
				c_zip <- rnd_zip
				let c_loc = Location [c_street1,c_street2] c_city c_state c_zip
				c_phone <- n_string (16,16)
				c_since <- getCurrentTime
				c_credit <- if c_id `List.elem` cs_oris then return "BC" else return "GC"
				c_disc <- generate $ choose (0,0.5)
				c_data <- a_string (300,500)
				Pads.printFileRep c_path $ CustomerInfo c_name c_loc c_phone c_since c_credit 50000 c_disc (-10) 10 1 0 c_data
				
				he_id <- uniqueFileName
				let he_path = h_path </> he_id
				h_date <- getCurrentTime
				h_data <- a_string (12,24)
				Pads.printFileRep he_path $ HistoryEntryInfo c_id d_id w_id d_id w_id h_date 10 h_data
			mapM_ populateCustomer cs_ids
			
			let os_path = d_path </> "orders"
			createDirectoryIfMissing True os_path
			let os_ids = [1..3000]
			os_c_ids :: [Integer] <- shuffleM [1..3000]
			let populateOrder (o_id,o_c_id) = do
				let o_path = os_path </> show o_id
				let o_c_tgt = "../../Customers" </> show o_c_id
				let o_c_path = o_path </> addExtension (show o_c_id) "Customer"
				createSymbolicLink o_c_tgt o_c_path
				
				let o_info_path = o_path </> "orderInfo"
				o_entry_d <- getCurrentTime
				o_carrier_id <- if o_id < 2101 then liftM Just (generate $ choose (1,10)) else return Nothing
				o_ol_cnt <- generate $ choose (5,15)
				Pads.printFileRep o_info_path $ OrderInfo o_entry_d o_carrier_id o_ol_cnt True
				
				let ols_path = o_path </> "orderLines"
				createDirectoryIfMissing True ols_path
				let populateOrderLine ol_id = do
					let ol_path = ols_path </> show ol_id
					ol_i_id :: Integer <- generate $ choose (1,100000)
					let ol_i_tgt = "../../../Stock" </> show ol_i_id
					let ol_i_path = ol_path </> addExtension (show ol_i_id) "Item"
					createSymbolicLink ol_i_tgt ol_i_path
					
					let ol_supply_w_tgt = "../../../../Warehouses" </> show w_id
					let ol_supply_w_path = ol_path </> addExtension (show w_id) "Warehouse"
					createSymbolicLink ol_supply_w_tgt ol_supply_w_path
					
					let ol_info_path = ol_path </> "orderLineInfo"
					let ol_delivery_d = if o_id < 2101 then Just o_entry_d else Nothing
					ol_amount <- if o_id < 2101 then return 0 else generate $ choose (0.1,9999.99)
					ol_dist_info <- l_string (24,24)
					Pads.printFileRep ol_info_path $ OrderLineInfo ol_id ol_delivery_d 5 ol_amount ol_dist_info
				mapM_ populateOrderLine [1..fromIntegral o_ol_cnt]
				
			mapM_ populateOrder $ zip os_ids os_c_ids
			
			let nos_path = d_path </> "newOrders"
			createDirectoryIfMissing True nos_path
			let populateNewOrder no_id = return () -- XXX: add new flag
			mapM_ populateNewOrder [1..900]
			
		mapM_ populateDistrict [1..10]
		
		let s_path = ws_path </> "stock"
		createDirectoryIfMissing True s_path
		let s_is_ids = [1..100000]
		s_is_oris <- generate $ vectorOf 10000 $ elements s_is_ids
		let populateStockItem s_i_id = do
			let s_i_path = s_path </> show s_i_id
			s_i_quantity <- generate $ choose (10,100)
			s_i_dists <- generate $ vectorOf 10 $ l_string' (24,24)
			s_i_data <- if s_i_id `List.elem` s_is_oris
				then do
					l1 <- generate $ choose (9,21)
					str1 <- a_string (l1,l1)
					str2 <- a_string (18-l1-8,42-l1)
					return $ str1 ++ "ORIGINAL" ++ str2
				else a_string (26,50)
			Pads.printFileRep s_i_path $ StockItemInfo s_i_quantity s_i_dists 0 0 0 s_i_data
		mapM_ populateStockItem s_is_ids
	mapM_ populateWarehouse [1..w]
	
	let is_path = root_db </> "items"
	createDirectoryIfMissing True is_path
	let is_ids = [1..100000]
	is_oris <- generate $ vectorOf 10000 $ elements is_ids
	let populateItem i_id = do
		let i_path = is_path </> show i_id
		i_img <- a_string (5,15)
		i_name <- a_string (14,24)
		i_price <- generate $ choose (1,100)
		i_data <- if i_id `List.elem` is_oris
			then do
				l1 <- generate $ choose (9,21)
				str1 <- a_string (l1,l1)
				str2 <- a_string (18-l1-8,42-l1)
				return $ str1 ++ "ORIGINAL" ++ str2
			else a_string (26,50)
		Pads.printFileRep i_path $ ItemInfo i_img i_name i_price i_data
	mapM_ populateItem is_ids
	
-- * Transactions

root_db = "company"

type NewOrderInput = (Id,Id,Id,Map Id (Id,Unsigned_2))

-- One non-repeating group of fields: W_ID, D_ID, C_ID, O_ID, O_OL_CNT, C_LAST, C_CREDIT,
-- C_DISCOUNT, W_TAX, D_TAX, O_ENTRY_D, total_amount, and an optional execution status message other
-- than "Item number is not valid".
type NewOrderOutput = (Id,Id,Id,Id,Unsigned_2,VText,FText,Signed_4_4,Signed_4_4,Signed_4_4,DateTime,Signed_6_2,[NewOrderOutputLine])
-- One repeating group of fields: OL_SUPPLY_W_ID, OL_I_ID, I_NAME, OL_QUANTITY, S_QUANTITY,
-- brand_generic, I_PRICE, and OL_AMOUNT. The group is repeated O_OL_CNT times (once per item in the
-- order), equal to the computed value of ol_cnt.
type NewOrderOutputLine = (Id,Id,VText,Unsigned_2,Signed_4,String,Signed_5_2,Signed_6_2)

-- input data generation for @newOrder@
genNewOrder :: Integer -> Gen NewOrderInput
genNewOrder w = do
	w_id <- choose (1,w)
	d_id <- choose (1,10)
	c_id <- nuRand (1023,c1023,1,3000)
	ol_cnt :: Int <- choose (5,15)
	rbk :: Int <- choose (1,100)
	let genOrder xs i = do
		ol_i_id <- if (i == ol_cnt && rbk == 1)
			then return (-1)
			else nuRand (8191,c8191,1,100000)
		ol_supply_w_id <- frequency [(99,return w_id),(1,suchThat (choose (1,w)) (/= w_id))]
		ol_quantity <- choose (1,10)
		return $ Map.insert ol_i_id (ol_supply_w_id,ol_quantity) xs	
	xs <- Foldable.foldlM genOrder Map.empty [1..ol_cnt::Int]
	return (w_id,d_id,c_id,xs)

-- For a given warehouse number (W_ID), district number (D_W_ID , D_ID), customer number (C_W_ID
-- , C_D_ID , C_ ID), count of items (ol_cnt, not communicated to the SUT), and for a given set of items (OL_I_ID),
-- supplying warehouses (OL_SUPPLY_W_ID), and quantities (OL_QUANTITY):
newOrder :: NewOrderInput -> IO NewOrderOutput
newOrder (w_id,d_id,c_id,order) = atomically $ do
	
	-- The row in the WAREHOUSE table with matching W_ID is selected and W_TAX, the warehouse tax rate, is
	-- retrieved.
	
	co_var :: Company TxVarFS <- new () root_db
	(co_md,co) <- read co_var
	(ws_md,ws) <- read (warehouses co)
	w_var <- lookupEntry w_id ws
	(w_md,w) <- read w_var
	let (w_info_md,w_info) = warehouseInfo w
	let w_tax = warehouseTax w_info
	
	-- The row in the DISTRICT table with matching D_W_ID and D_ID is selected, D_TAX, the district tax rate, is
	-- retrieved, and D_NEXT_O_ID, the next available order number for the district, is retrieved and incremented
	-- by one.
	
	(ds_md,ds) <- read (districts w)
	d_var <- lookupEntry d_id ds
	(d_md,d) <- read d_var
	let (d_info_md,d_info) = districtInfo d
	let d_tax = districtTax d_info
	let d_next_o = succ $ districtNextOrder d_info 
	writeOrThrow d_var (d_md,d { districtInfo = (d_info_md,d_info { districtNextOrder = d_next_o }) }) WriteFailed
	
	-- The row in the CUSTOMER table with matching C_W_ID, C_D_ID, and C_ID is selected and C_DISCOUNT,
	-- the customer's discount rate, C_LAST, the customer's last name, and C_CREDIT, the customer's credit status,
	-- are retrieved.
	
	(cs_md,cs) <- read (customers d)
	c_var <- lookupEntry c_id cs
	(c_md,c)  <- read c_var
	let c_discount = customerDiscount c
	let c_last = lastName $ customerName c
	let c_credit = customerCredit c
	
	-- A new row is inserted into both the NEW-ORDER table and the ORDER table to reflect the creation of the
	-- new order. O_CARRIER_ID is set to a null value. If the order includes only home order-lines, then
	-- O_ALL_LOCAL is set to 1, otherwise O_ALL_LOCAL is set to 0.
	
	(os_md,os) <- read (orders d)
	let o_path = fullpath os_md </> printRep d_next_o
	o_var :: Order TxVarFS <- new () o_path
	let o_c_path = o_path </> addExtension (printRep c_id) "Customer"
	let o_c_path_tgt = o_c_path </> "../../Customers" </> (printRep c_id)
	let o_c = ((cleanSymLinkFileInfo o_c_path o_c_path_tgt,cleanBasePD),o_c_path_tgt)
	let o_local = and $ map ((==w_id) . fst) $ Map.elems order
	let o_cnt = toEnum $ Map.size order
	current_time <- unsafeIOToFTM getCurrentTime
	let o_info = OrderInfo current_time Nothing o_cnt o_local
	let o_info_md = (cleanFileInfo $ o_path </> "orderInfo",Pads.defaultMd o_info)
	let o_ls_path = o_path </> "orderLines"
	o_ls_var <- new () o_ls_path
	writeOrThrow o_ls_var (cleanFileInfo o_ls_path,Map.empty) WriteFailed -- make sure to create the orderLines folder
	let o = Order_inner o_c o_ls_var (o_info_md,o_info)
	writeOrThrow o_var (cleanFileInfo o_path,o) WriteFailed
	
	-- For each O_OL_CNT item on the order:
	
	let newOrderItem (ol_ix,(ol_i_id,(ol_supply_w_id,ol_quantity))) = do
		
		-- The row in the ITEM table with matching I_ID (equals OL_I_ID) is selected and I_PRICE, the price of the
		-- item, I_NAME, the name of the item, and I_DATA are retrieved.
	
		(is_md,is) <- read (items co)
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
		(s_md,s) <- read (stock ol_supply_w)
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
		
		let brand_generic = if ("ORIGINAL" `isInfixOf` i_data && "ORIGINAL" `isInfixOf` s_i_data) then "B" else "G"
	
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

type PaymentInput = (Id,Id,(Id,Id,Either Id VText),Signed_6_2)

-- The following fields are displayed: W_ID,
-- D_ID, C_ID, C_D_ID, C_W_ID, W_LOCATION, D_LOCATION, C_PERSON, C_LOCATION, C_PHONE, C_SINCE, C_CREDIT, C_CREDIT_LIM, C_DISCOUNT, C_BALANCE, the first 200
-- characters of C_DATA (only if C_CREDIT = "BC"), H_AMOUNT, and H_DATE.
type PaymentOutput = (Id,Id,Id,Id,Location,Location,Person,Location,FText,DateTime,FText,Signed_12_2,Signed_4_4,Signed_12_2,Maybe VText,Signed_6_2,DateTime)

genPayment :: Integer -> Gen PaymentInput
genPayment w = do
	w_id <- choose (1,w)
	d_id <- choose (1,10)
	c_id_or_last <- frequency
		[ (60,liftM (Right . lastNameFromInt . fromIntegral) $ nuRand (255,c255,0,999))
		, (40,liftM Left $ nuRand (1023,c1023,1,3000))
		]
	let remoteCustomer = do
		c_w_id <- suchThat (choose (1,w)) (/= w_id)
		c_d_id <- choose (1,10)
		return (c_w_id,c_d_id)
	(c_w_id,c_d_id) <- frequency [(85,return (w_id,d_id)),(15,remoteCustomer)]
	h_amount <- choose (1,5000)
	return (w_id,d_id,(c_w_id,c_d_id,c_id_or_last),h_amount)

-- For a given warehouse number (W_ID), district number (D_W_ID , D_ID), customer number (C_W_ID
-- , C_D_ID , C_ ID) or customer last name (C_W_ID , C_D_ID , C_LAST), and payment amount (H_AMOUNT):
payment :: PaymentInput -> IO PaymentOutput
payment (w_id,d_id,(c_w_id,c_d_id,c_id_or_last),h_amount) = atomically $ do
	
	-- The row in the WAREHOUSE table with matching W_ID is selected. W_NAME, W_STREET_1,
	-- W_STREET_2, W_CITY, W_STATE, and W_ZIP are retrieved and W_YTD, the warehouse's year-to-date
	-- balance, is increased by H_ AMOUNT.
	
	co_var :: Company TxVarFS <- new () root_db
	(co_md,co) <- read co_var
	(ws_md,ws) <- read (warehouses co)
	w_var <- lookupEntry w_id ws
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
	
	(ds_md,ds) <- read (districts w)
	d_var <- lookupEntry d_id ds
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
	(c_ds_md,c_ds) <- read (districts c_w)
	c_d_var <- lookupEntry c_d_id c_ds
	(c_d_md,c_d) <- read c_d_var
	(cs_md,cs) <- read (customers c_d)
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
	
	let h_path = fullpath co_md </> "history"
	h_entry_path <- liftM (h_path </>) (unsafeIOToFTM uniqueFileName)
	h_entry_var :: HistoryEntry TxVarFS <- new () h_entry_path
	h_date <- unsafeIOToFTM getCurrentTime
	let h_entry = HistoryEntryInfo c_id c_d_id c_w_id d_id w_id h_date h_amount h_data
	let h_entry_bmd = Pads.defaultMd h_entry
	writeOrThrow h_entry_var ((cleanFileInfo h_entry_path,h_entry_bmd),h_entry) WriteFailed
	
	let ret_c_data = if (c_credit == "BC") then Just (take 200 c_data') else Nothing
	return (d_id,c_id,c_d_id,c_w_id,w_loc,d_loc,c_person,c_loc,c_phone,c_since,c_credit,c_credit_lim,c_discount,c_balance',ret_c_data,h_amount,h_date)
	
type OrderStatusInput = (Id,Id,Either Id VText)

-- One non-repeating group of fields: W_ID, D_ID, C_ID, C_Person, C_BALANCE, O_ID,
-- O_ENTRY_D, and O_CARRIER_ID; 
type OrderStatusOutput = (Id,Id,Id,Person,Signed_12_2,Id,DateTime,Maybe Id,[OrderStatusOutputLine])
--One repeating group of fields: OL_SUPPLY_W_ID, OL_I_ID, OL_QUANTITY, OL_AMOUNT, and
-- OL_DELIVERY_D. The group is repeated O_OL_CNT times (once per item in the order).
type OrderStatusOutputLine = (Id,Id,Unsigned_2,Signed_6_2,Maybe DateTime)

genOrderStatus :: Integer -> Gen OrderStatusInput
genOrderStatus w = do
	w_id <- choose (1,w)
	d_id <- choose (1,10)
	c_id_or_last <- frequency
		[ (60,liftM (Right . lastNameFromInt . fromIntegral) $ nuRand (255,c255,0,999))
		, (40,liftM Left $ nuRand (1023,c1023,1,3000))
		]
	return (w_id,d_id,c_id_or_last)

-- For a given customer number (C_W_ID , C_D_ID , C_ ID):
orderStatus :: OrderStatusInput -> IO OrderStatusOutput
orderStatus (w_id,d_id,c_id_or_last) = atomically $ do
	
	co_var :: Company TxVarFS <- new () root_db
	(co_md,co) <- read co_var
	(ws_md,ws) <- read (warehouses co)
	w_var <- lookupEntry w_id ws
	(w_md,w) <- read w_var
	(ds_md,ds) <- read (districts w)
	d_var <- lookupEntry d_id ds
	(d_md,d) <- read d_var
	(cs_md,cs) <- read (customers d)
	
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
	
	(os_md,os) <- read (orders d)
	let (o_id,o_var) = Map.findMax os
	(o_md,o) <- read o_var
	let (o_info_md,o_info) = orderInfo o
	let o_entry_d = orderEntryDate o_info
	let o_carrier_id = orderCarrier o_info
	
	-- All rows in the ORDER-LINE table with matching OL_W_ID (equals O_W_ID), OL_D_ID (equals O_D_ID),
	-- and OL_O_ID (equals O_ID) are selected and the corresponding sets of OL_I_ID, OL_SUPPLY_W_ID,
	-- OL_QUANTITY, OL_AMOUNT, and OL_DELIVERY_D are retrieved.
	
	(ols_md,ols) <- read (orderLines o)
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

type DeliveryInput = (Id,Id)

genDelivery :: Integer -> Gen DeliveryInput
genDelivery w = do
	w_id <- choose (1,w)
	o_carrier_id <- choose (1,10)
	return (w_id,o_carrier_id)

-- For a given warehouse number (W_ID) and for a given carrier number (O_CARRIER_ID):
delivery :: DeliveryInput -> IO ()
delivery (w_id,o_carrier_id) = atomically $ do
	
	co_var :: Company TxVarFS <- new () root_db
	(co_md,co) <- read co_var
	(ws_md,ws) <- read (warehouses co)
	w_var <- lookupEntry w_id ws
	(w_md,w) <- read w_var
	(ds_md,ds) <- read (districts w)
	
	-- For each of the 10 districts (D_W_ID , D_ID) within that warehouse
	let deliveryDistrict d_var = do
		(d_md,d) <- read d_var
		(nos_md,nos) <- read (newOrders d)
	
		-- The row in the NEW-ORDER table with matching NO_W_ID (equals W_ID) and NO_D_ID (equals D_ID)
		-- and with the lowest NO_O_ID value is selected. This is the oldest undelivered order of that district.
		-- NO_O_ID, the order number, is retrieved. If no matching row is found, then the delivery of an order for this
		-- district is skipped.
	
		case Set.minView nos of
			Nothing -> return ()
			Just ((no_id,no_var),nos') -> do
				
				-- The selected row in the NEW-ORDER table is deleted.
				writeOrThrow (newOrders d) (nos_md,nos') WriteFailed
				
				-- The row in the ORDER table with matching O_W_ID (equals W_ ID), O_D_ID (equals D_ID), and O_ID
				-- (equals NO_O_ID) is selected, O_C_ID, the customer number, is retrieved, and O_CARRIER_ID is updated.
				(os_md,os) <- read (orders d)
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
				(ols_md,ols) <- read (orderLines o)
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
				(cs_md,cs) <- read (customers d)
				c_var :: Customer TxVarFS <- lookupEntry o_c_id cs
				(c_md,c) <- read c_var
				let c_balance = customerBalance c
				let c_delivery_cnt = customerDeliveryCount c
				let c' = c { customerBalance = c_balance + realToFrac ols_amount, customerDeliveryCount = succ c_delivery_cnt }
				writeOrThrow c_var (c_md,c') WriteFailed
				
	Foldable.mapM_ deliveryDistrict ds

type StockLevelInput = (Id,Id,Signed_4)

--	The following fields are displayed: W_ID, D_ID, threshold, and low_stock.
type StockLevelOutput = (Id,Id,Signed_4,[(Id,StockItemInfo)])

genStockLevel :: Integer -> Gen StockLevelInput
genStockLevel w = do
	w_id <- choose (1,w)
	d_id <- choose (1,10)
	threshold <- choose (10,20)
	return (w_id,d_id,threshold)

-- 	For a given warehouse number (W_ID), district num ber (D_W_ID , D_ID), and stock level threshold:
stockLevel :: StockLevelInput -> IO StockLevelOutput
stockLevel (w_id,d_id,threshold) = atomically $ do
	
	-- The row in the DISTRICT table with matching D_W_ID and D_ID is selected and D_NEXT_O_ID is retrieved. 
	co_var :: Company TxVarFS <- new () root_db
	(co_md,co) <- read co_var
	(ws_md,ws) <- read (warehouses co)
	w_var <- lookupEntry w_id ws
	(w_md,w) <- read w_var
	(ds_md,ds) <- read (districts w)
	d_var <- lookupEntry d_id ds
	(d_md,d) <- read d_var
	let (d_info_md,d_info) = districtInfo d
	let d_next_o_id = districtNextOrder d_info
	
	-- All rows in the ORDER-LINE table with matching OL_W_ID (equals W_ID), OL_D_ID (equals D_ID), and
	-- OL_O_ID (lower than D_NEXT_O_ID and greater than or equal to D_NEXT_O_ID minus 20) are selected.
	-- They are the items for 20 recent orders of the district. 
	
	(os_md,os) <- read (orders d)
	let os_20 = Map.filterWithKey (\o_id o_var -> (d_next_o_id - 20) <= o_id && o_id < d_next_o_id) os
	let getOrderLineItems is ol_var = do
		(ol_md,ol) <- read ol_var
		let (ol_i_md,ol_i_tgt) = orderLineItem ol
		let ol_i = parseRep $ takeFileName ol_i_tgt
		return $ Set.insert ol_i is
	let getOrderItems is o_var = do
		(o_md,o) <- read o_var
		(ols_md,ols) <- read (orderLines o)
		Foldable.foldlM getOrderLineItems is ols
	is <- Foldable.foldlM getOrderItems Set.empty os_20
	
	-- All rows in the STOCK table with matching S_I_ID (equals OL_I_ID) and S_W_ID (equals W_ID) from the list
	-- of distinct item numbers and with S_QUANTITY lower than threshold are counted (giving low_stock).
	
	(s_md,s) <- read (stock w)
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


