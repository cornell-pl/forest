/* Notes:
   I did not distinguish alpha and alpha numeric types.

   This encoding does not capture the names associated with individual characters
   and does not check that only the expected characters appear.

   I wasn't sure how to handle the price denominator codes, so left them as characters.
   (p. 49)

   I wasn't sure what to do with the strike price tables (pp. 92-94), so left those
   fields as characters.

   I did not include the size constraint on administrative messages section 9.02

   I did not describe the various administrative and control formats.
   
 */

#define SOH   0x01  // = \001
#define ETX   0x03  // = \003
#define US    0x1F  // = \037
#define ETXorUS  "/[\003\037]/"

Ptypedef Pchar S(:Pchar x:) : S y => { y == x };

/**************************************/
/********** Header types **************/
/**************************************/


/* p. 14.  No 'j' category mentioned */
Punion Type_t(:Pchar category:){
  Pswitch (category) {
    Pcase 'a':  Pchar       eqiSaleTy;
    Pcase 'd':  S(:' ':)    openIntTy;
    Pcase 'f':  S(:' ':)    eqiSumTy;
    Pcase 'k':  Pchar       eqiQuoteTy : eqiQuoteTy != 'I';

    Pcase 'C':  S(:' ':)    adminTy;
    Pcase 'F':  S(:'P':)    fcoSumTy;
    Pcase 'H':  Pchar       controlTy;
    Pcase 'O':  Pchar       fcoSaleTy;
    Pcase 'U':  Pchar       fcoQuoteTy;
    Pcase 'Y':  Pchar       uvalueTy;
    Pdefault:   Pchar       otherTy;  // accept future extensions
  }
};

/* p. 14 */
Pstruct Id_t{
  Pchar                mcategory;
  Type_t(:mcategory:)  mtype;
};

/* p. 12 */
Pstruct Header_t {
  Pchar           pid : pid != 'B';
  Pchar           retransRequestor;  // ' ': original message
                                     // 'V': retransmission for all data recipients
  Id_t            mid;
  Puint32_FW(:8:) msn;               // message sequence number
  Ptime_explicit_FW(:6,"%H%M%S", P_cstr2timezone("-0500"):)  
                  tm;
};

/**************************************/
/************* Body types *************/
/**************************************/

/* p. 22 */
Pstruct LastSaleM_t{
  Pstring_FW(:5:) securitySymbol;
  Pstring_FW(:2:) reserved1;
  Pchar           expirationDate;
  Pchar           year;
  Pchar           strikePriceCode;
  Pchar           strikePriceDenominatorCode;
  Puint32_FW(:7:) explicitStrikePrice;
  Puint32_FW(:6:) volume;
  Pchar           premiumPriceDenominatorCode;
  Puint32_FW(:8:) premiumPrice;
  Pchar           sessionIndicator;
  Pchar           reserved2;
};

/* p. 26 */
Pstruct OpenInterest_t{
  Pstring_FW(:5:) securitySymbol;
  Pstring_FW(:2:) reserved1;
  Pchar           expirationDate;
  Pchar           year;
  Pchar           strikePriceCode;
  Pchar           strikePriceDenominatorCode;
  Puint32_FW(:7:) explicitStrikePrice;
  Puint32_FW(:7:) openInterestVolume;
  Pstring_FW(:2:) reserved2;
};


/* p. 30 */
Pstruct EquitySumM_t{
  Pstring_FW(:5:) securitySymbol;
  Pstring_FW(:2:) reserved1;
  Pchar           expirationDate;
  Pchar           year;
  Pchar           strikePriceCode;
  Pchar           strikePriceDenominatorCode;
  Puint32_FW(:7:) explicitStrikePrice;
  Puint32_FW(:6:) volume;
  Puint32_FW(:7:) openInterestVolume;
  Pchar           premiumPriceDenominatorCode;
  Puint32_FW(:8:) openPrice;
  Puint32_FW(:8:) highPrice;
  Puint32_FW(:8:) lowPrice;
  Puint32_FW(:8:) lastPrice;
  Pchar           netChangeIndicator;
  Puint32_FW(:8:) netChange;
  Pchar           underlyingPriceDenominatorCode;
  Puint64_FW(:11:)underlyingStockPrice;
  Puint32_FW(:8:) bidPrice;
  Puint32_FW(:8:) offerPrice;
  Pstring_FW(:2:) reserved2;
};

/* p. 24 */
Pstruct BestBidAppendage_t {
  Pchar           bestBidPid;
  Pchar           bestBidPremiumPriceDenominatorCode;
  Puint32_FW(:8:) bestBidPrice;
  Puint32_FW(:5:) bestBidSize;
  Pchar           reserved;
};

/* p. 25 */
Pstruct BestOfferAppendage_t {
  Pchar           bestOfferPid;
  Pchar           bestOfferPremiumPriceDenominatorCode;
  Puint32_FW(:8:) bestOfferPrice;
  Puint32_FW(:5:) bestOfferSize;
  Pchar           reserved;
};

Punion BBidAppendage(:Pchar bbo:){
  Pswitch (bbo) {
    Pcase 'M':  BestBidAppendage_t  bba1;
    Pcase 'N':  BestBidAppendage_t  bba2;
    Pcase 'O':  BestBidAppendage_t  bba3;
    Pcase 'P':  BestBidAppendage_t  bba4;
    Pdefault:   Pcompute Pomit Pint32 t = 0;
  }
};

Punion BOfferAppendage(:Pchar bbo:){
  Pswitch (bbo){
    Pcase 'C':  BestOfferAppendage_t boa1;
    Pcase 'G':  BestOfferAppendage_t boa2;
    Pcase 'K':  BestOfferAppendage_t boa3;
    Pcase 'O':  BestOfferAppendage_t boa4;
    Pdefault:   Pcompute Pomit Pint32 t = 0;
  }
};

/* p. 23 */
Pstruct QuoteM_t{
  Pstring_FW(:5:) securitySymbol;
  Pstring_FW(:2:) reserved1;
  Pchar           expirationDate;
  Pchar           year;
  Pchar           strikePriceCode;
  Pchar           strikePriceDenominatorCode;
  Puint32_FW(:7:) explicitStrikePrice;
  Pchar           premiumPriceDenominatorCode;
  Puint32_FW(:8:) bidPrice;
  Puint32_FW(:5:) bidSize;
  Puint32_FW(:8:) offerPrice;
  Puint32_FW(:5:) offerSize;
  Pchar           sessionIndicator;
  Pchar           bboIndicator;
  BBidAppendage(:bboIndicator:)   bestBidApp;
  BOfferAppendage(:bboIndicator:) bestOfferApp;
};

/* p. 33 */
Pstruct FCOSumM_t{
  Pstring_FW(:3:) securitySymbol;
  Pchar           expirationDate;
  Pchar           strikePriceCode;
  Puint32_FW(:4:) volume;
  Puint32_FW(:7:) openInterestVolume;
  Puint32_FW(:4:) openPrice;
  Puint32_FW(:4:) highPrice;
  Puint32_FW(:4:) lowPrice;
  Puint32_FW(:4:) lastPrice;
  Pchar           netChangeIndicator;
  Puint32_FW(:4:) netChange;
  Puint32_FW(:5:) underlyingStockPrice;
  Pchar           reserved2;
};


/* p. 31 */
Pstruct FCOLastSaleM_t{
  Pstring_FW(:3:) securitySymbol;
  Pchar           expirationDate;
  Pchar           strikePriceCode;
  Puint32_FW(:4:) volume;
  Puint32_FW(:4:) premiumPrice;
};

/* p. 32 */
Pstruct FCOQuoteM_t{
  Pstring_FW(:3:) securitySymbol;
  Pchar           expirationDate;
  Pchar           strikePriceCode;
  Puint32_FW(:4:) bidPrice;
  Puint32_FW(:4:) offerPrice;
};


/* p. 27 */
Pstruct LastSaleIndexGroup_t{
  Pstring_FW(:3:) indexSymbol;
  Pstring_FW(:2:) reserved;
  Puint32_FW(:8:) indexValue;
};

/* p. 27 */
Pstruct ValueMLastSale_t{
  Puint32_FW(:2:)              number;
  LastSaleIndexGroup_t[number] groups;
};

/* p. 28 */
Pstruct OfferIndexGroup_t{
  Pstring_FW(:3:) indexSymbol;
  Pstring_FW(:2:) reserved;
  Puint32_FW(:8:) bidIndexValue;
  Puint32_FW(:8:) offerIndexValue;
};

/* p. 28 */
Pstruct ValueMOffer_t{
  Puint32_FW(:2:)           number;
  OfferIndexGroup_t[number] groups;
};


/* p. 29 */
Pstruct SpotIndexGroup_t{
  Pstring_FW(:3:) fcoSymbol;
  Puint8_FW (:2:) decimalPlacementIndicator;
  Puint32_FW(:8:) foreignCurrencySpotValue;
};

/* p. 29 */
Pstruct ValueMSpot_t{
  Puint32_FW(:2:)          number;
  SpotIndexGroup_t[number] groups;
};

Punion ValueM_t(:Pchar val:){
  Pswitch (val) {
    Pcase ' ' : ValueMLastSale_t lastSale;
    Pcase 'I' : ValueMOffer_t    offer;
    Pcase 'F' : ValueMSpot_t     foreign;
    Pcase 'C' : ValueMSpot_t     closing;
  }
};

Punion Body_t(:Header_t h:) {
  Pswitch (h.mid.mcategory) {
    Pcase 'a':  LastSaleM_t     eqiSaleM; 
    Pcase 'd':  OpenInterest_t  openIntM; 
    Pcase 'f':  EquitySumM_t    eqiSumM;
    Pcase 'k':  QuoteM_t        eqiQuoteM; 

    Pcase 'C':  Pstring_SE(:ETXorUS:)   adminM;
    Pcase 'F':  FCOSumM_t               fcoSumM;
    Pcase 'H':  Pstring_SE(:ETXorUS:)   controlM;
    Pcase 'O':  FCOLastSaleM_t          fcoSaleM;
    Pcase 'U':  FCOQuoteM_t             fcoQuoteM;
    Pcase 'Y':  ValueM_t(:h.mid.mtype.val.uvalueTy:) 
                                        uvalueM;
    Pdefault:   Pstring_SE(:ETXorUS:)   otherM; // accept extensions
  }
};

Pstruct Message_t{
  Header_t      h;
  Body_t(:h:)   b;
};

Parray block_t {
   Message_t[] : Psep(US) && Pterm(ETX);
};

Pstruct Transmission_t{
  SOH;
  block_t b;
  ETX;
} Pwhere {
  Pparsecheck(end.offset - begin.offset <= 1000);
};


