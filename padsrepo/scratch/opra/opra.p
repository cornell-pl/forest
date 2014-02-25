/* Notes:
  
   All page numbers mentioned might be off due to changes in the documentation. --YHM.

   I did not distinguish alpha and alpha numeric types.

   This encoding captures the names associated with individual characters
   and checks that only the expected characters appear.

   I wasn't sure how to handle the price denominator codes, so left them as characters.
   (p. 49)

   I wasn't sure what to do with the strike price tables (pp. 92-94), so left those
   fields as characters.

   I did not include the size constraint on administrative messages section 9.02

   I did not describe the various administrative and control formats, section 10.01.
   
 */

#define SOH   0x01  // = \001
#define ETX   0x03  // = \003
#define US    0x1F  // = \037
#define ETXorUS  "/[\003\037]/"

/* Singleton character: matches argument exactly */
Ptypedef Pchar S(:Pchar x:) : S y => { y == x };

/**************************************/
/********** Header types **************/
/**************************************/

/* p. 12 */
Penum Participant_t {
  ASE  Pfrom("A"),  // American Stock Exchange
  BSE  Pfrom("B"),  // Boston Stock Exchange; INACTIVE
  CBOE Pfrom("C"),  // Chicago Board Options Exchange
  ISE  Pfrom("I"),  // International Securities Exchange
  OPRA Pfrom("O"),  // Options Price Reporting Authority; message sent by SIAC for OPRA
  PE   Pfrom("P"),  // Pacific Exchange
  PSE  Pfrom("X")   // Philadelphia Stock Exchange
};

/* p. 15 */
Penum LastSale_t {
  REGULAR    Pfrom(" "),
  CANC       Pfrom("A"),
  OSEQ       Pfrom("B"),
  CNCL       Pfrom("C"),
  LATE       Pfrom("D"),
  CNCO       Pfrom("E"),
  OPEN       Pfrom("F"),
  CNOL       Pfrom("G"),
  OPNL       Pfrom("H"),
  AUTO       Pfrom("I"),
  REOP       Pfrom("J"),
  AJST       Pfrom("K"),
  SPRD       Pfrom("L"),
  STDL       Pfrom("M"),
  STPD       Pfrom("N"),
  CSTP       Pfrom("O"),
  BWRT       Pfrom("P"),
  CMBO       Pfrom("Q")
};

/* p. 16 */
Penum Control_t{
  TestCycleStart Pfrom("A"),
  TestCycleEnd   Pfrom("B"),
  StartDay       Pfrom("C"),
  GoodMorning    Pfrom("D"),
  StartSummary   Pfrom("E"),
  EndSummary     Pfrom("F"),
  EarlyMktClose  Pfrom("G"),
  EndTransRpt    Pfrom("H"),
  GoodNight      Pfrom("I"),
  EndDay         Pfrom("J"),
  ResetSqnNum    Pfrom("K"),
  StartOpenInt   Pfrom("L"),
  EndOpenInt     Pfrom("M"),
  StaleQuote     Pfrom("S")
};

/* p. 17 */
/* ?? Used for category j Equity and Index Quote; but no such on p. 14 */
Penum Quote_t{
  RegularTrade   Pfrom(" "),
  NonFirmQuote   Pfrom("F"),
  Rotation       Pfrom("R"),
  TradeHalted    Pfrom("T"),
  AutoExecElig   Pfrom("A"),
  Inactive       Pfrom("I"), //applies only to category U FCO Quote
  SpecialBookBid Pfrom("B"),
  SpecialBookOff Pfrom("O"),
  SpecialBookBO  Pfrom("C")
};

/* p. 17 Underlying value message */
Penum Value_t {
  LastSaleIndex  Pfrom(" "),
  BidOfferIndex  Pfrom("I"),
  ForeignCSpot   Pfrom("F"),
  ClosingSpot    Pfrom("C")
};


/* p. 14.  No 'j' category mentioned */
Penum Category_t {
  EquityIndexLastSale        Pfrom("a"),
  OpenInterest               Pfrom("d"), //p. 20 says 'D'
  EquityIndexEndOfDaySummary Pfrom("f"), 
  EquityIndexQuoteWithSize   Pfrom("k"), //p. 21 says 'K'

  Administrative             Pfrom("C"),
  FCOEndOfDaySummary         Pfrom("F"),
  Control                    Pfrom("H"),
  FCOLastSale                Pfrom("O"),
  FCOQuote                   Pfrom("U"),
  UnderlyingValue            Pfrom("Y")
};

Punion Type_t(:Category_t category:){
  Pswitch (category) {
    Pcase EquityIndexLastSale:         LastSale_t  eqiSaleTy;
    Pcase OpenInterest:                S(:' ':)    openIntTy;
    Pcase EquityIndexEndOfDaySummary:  S(:' ':)    eqiSumTy;
    Pcase EquityIndexQuoteWithSize:    Quote_t     eqiQuoteTy : eqiQuoteTy != Inactive;

    Pcase Administrative:      S(:' ':)       adminTy;
    Pcase FCOEndOfDaySummary:  S(:'P':)       fcoSumTy;
    Pcase Control:             Control_t      controlTy;
    Pcase FCOLastSale:         LastSale_t     fcoSaleTy;
    Pcase FCOQuote:            Quote_t        fcoQuoteTy;
    Pcase UnderlyingValue:     Value_t        uvalueTy;
    Pdefault:                  Pchar          otherTy;  // accept future extensions
  }
};

/* p. 14 */
Pstruct Id_t{
  Category_t           mcategory;
  Type_t(:mcategory:)  mtype;
};

/* p. 12 */
Pstruct Header_t {
  Participant_t   pid : pid != BSE;
  Pchar           retransRequestor;  // ' ': original message
                                     // 'V': retransmission for all data recipients
  Id_t            mid;
  Puint32_FW(:8:) msn;               // message sequence number
  Ptime_explicit_FW(:6,"%H%M%S", P_cstr2timezone("-0500"):)  
                  tm;
};

/**************************************/
/************ Field types *************/
/**************************************/
Penum ExpDate_t{
  C_JAN  Pfrom("A"),
  C_FEB  Pfrom("B"),
  C_MAR  Pfrom("C"),
  C_APR  Pfrom("D"),
  C_MAY  Pfrom("E"),
  C_JUN  Pfrom("F"),
  C_JUL  Pfrom("G"),
  C_AUG  Pfrom("H"),
  C_SEP  Pfrom("I"),
  C_OCT  Pfrom("J"),
  C_NOV  Pfrom("K"),
  C_DEC  Pfrom("L"),
  P_JAN  Pfrom("M"),
  P_FEB  Pfrom("N"),
  P_MAR  Pfrom("O"),
  P_APR  Pfrom("P"),
  P_MAY  Pfrom("Q"),
  P_JUN  Pfrom("R"),
  P_JUL  Pfrom("S"),
  P_AUG  Pfrom("T"),
  P_SEP  Pfrom("U"),
  P_OCT  Pfrom("V"),
  P_NOV  Pfrom("W"),
  P_DEC  Pfrom("X")
};

Penum ExpYear_t{
  Y_UND  Pfrom(" "),
  Y_0    Pfrom("0"),
  Y_1    Pfrom("1"),
  Y_2    Pfrom("2"),
  Y_3    Pfrom("3"),
  Y_4    Pfrom("4"),
  Y_5    Pfrom("5"),
  Y_6    Pfrom("6"),
  Y_7    Pfrom("7"),
  Y_8    Pfrom("8"),
  Y_9    Pfrom("9")
};

Penum ChangeInd_t {
  POS   Pfrom("+"),
  NEG   Pfrom("-"),
  ZER   Pfrom("0")
};

Penum SessionInd_t{
  AM     Pfrom("a"),
  NORMAL Pfrom(" ")
};


/* p. 34, 35, and 36 */
Penum BBOInd_t{
  NoBBChangeNoBOfferChange Pfrom("A"),
  NoBBChangeBOApp          Pfrom("C"),
  NoBBChangeNoBO           Pfrom("D"),
  QuoteBBNoBOChange        Pfrom("E"),
  QuoteBBQuoteBO           Pfrom("F"),
  QuoteBBBOApp             Pfrom("G"),
  QuoteBBNoBO              Pfrom("H"),
  NoBBNoBOChange           Pfrom("I"),
  NoBBQuoteBO              Pfrom("J"),
  NoBBBOApp                Pfrom("K"),
  NoBBNoBO                 Pfrom("L"),
  BBAppNoBOChange          Pfrom("M"),
  BBAppQuoteBO             Pfrom("N"),
  BBAppBOApp               Pfrom("O"),
  BBAppNoBO                Pfrom("P")
};


/**************************************/
/************* Body types *************/
/**************************************/

/* p. 22 */
Pstruct LastSaleM_t{
  Pstring_FW(:5:) securitySymbol;
  Pstring_FW(:2:) reserved1;
  ExpDate_t       expirationDate;
  ExpYear_t       year;
  Pchar           strikePriceCode;
  Pchar           strikePriceDenominatorCode;
  Puint32_FW(:7:) explicitStrikePrice;
  Puint32_FW(:6:) volume;
  Pchar           premiumPriceDenominatorCode;
  Puint32_FW(:8:) premiumPrice;
  SessionInd_t    sessionIndicator;
  Pchar           reserved2;
};

/* p. 26 */
Pstruct OpenInterest_t{
  Pstring_FW(:5:) securitySymbol;
  Pstring_FW(:2:) reserved1;
  ExpDate_t       expirationDate;
  ExpYear_t       year;
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
  ExpDate_t       expirationDate;
  ExpYear_t       year;
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
  ChangeInd_t     netChangeIndicator;
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

Punion BBidAppendage(:BBOInd_t bbo:){
  Pswitch (bbo) {
    Pcase BBAppNoBOChange:         BestBidAppendage_t  bba1;
    Pcase BBAppQuoteBO:            BestBidAppendage_t  bba2;
    Pcase BBAppBOApp:              BestBidAppendage_t  bba3;
    Pcase BBAppNoBO:               BestBidAppendage_t  bba4;
    Pdefault:                      Pcompute Pomit Pint32 t = 0;
  }
};

Punion BOfferAppendage(:BBOInd_t bbo:){
  Pswitch (bbo){
    Pcase NoBBChangeBOApp:         BestOfferAppendage_t boa1;
    Pcase QuoteBBBOApp:            BestOfferAppendage_t boa2;
    Pcase NoBBBOApp:               BestOfferAppendage_t boa3;
    Pcase BBAppBOApp:              BestOfferAppendage_t boa4;
    Pdefault:                      Pcompute Pomit Pint32 t = 0;
  }
};

/* p. 23 */
Pstruct QuoteM_t{
  Pstring_FW(:5:) securitySymbol;
  Pstring_FW(:2:) reserved1;
  ExpDate_t       expirationDate;
  ExpYear_t       year;
  Pchar           strikePriceCode;
  Pchar           strikePriceDenominatorCode; /* 'A'-'F' */
  Puint32_FW(:7:) explicitStrikePrice;
  Pchar           premiumPriceDenominatorCode;
  Puint32_FW(:8:) bidPrice;
  Puint32_FW(:5:) bidSize;
  Puint32_FW(:8:) offerPrice;
  Puint32_FW(:5:) offerSize;
  SessionInd_t    sessionIndicator;
  BBOInd_t        bboIndicator;
  BBidAppendage(:bboIndicator:)   bestBidApp;
  BOfferAppendage(:bboIndicator:) bestOfferApp;
};

/* p. 33 */
Pstruct FCOSumM_t{
  Pstring_FW(:3:) securitySymbol;
  ExpDate_t       expirationDate;
  Pchar           strikePriceCode;
  Puint32_FW(:4:) volume;
  Puint32_FW(:7:) openInterestVolume;
  Puint32_FW(:4:) openPrice;
  Puint32_FW(:4:) highPrice;
  Puint32_FW(:4:) lowPrice;
  Puint32_FW(:4:) lastPrice;
  ChangeInd_t     netChangeIndicator;
  Puint32_FW(:4:) netChange;
  Puint32_FW(:5:) underlyingStockPrice;
  Pchar           reserved2;
};


/* p. 31 */
Pstruct FCOLastSaleM_t{
  Pstring_FW(:3:) securitySymbol;
  ExpDate_t       expirationDate;
  Pchar           strikePriceCode;
  Puint32_FW(:4:) volume;
  Puint32_FW(:4:) premiumPrice;
};

/* p. 32 */
Pstruct FCOQuoteM_t{
  Pstring_FW(:3:) securitySymbol;
  ExpDate_t       expirationDate;
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

Punion ValueM_t(:Value_t val:){
  Pswitch (val) {
    Pcase LastSaleIndex : ValueMLastSale_t lastSale;
    Pcase BidOfferIndex : ValueMOffer_t    offer;
    Pcase ForeignCSpot  : ValueMSpot_t     foreign;
    Pcase ClosingSpot   : ValueMSpot_t     closing;
  }
};

Punion Body_t(:Header_t h:) {
  Pswitch (h.mid.mcategory) {
    Pcase EquityIndexLastSale:         LastSaleM_t     eqiSaleM; 
    Pcase OpenInterest:                OpenInterest_t  openIntM; 
    Pcase EquityIndexEndOfDaySummary:  EquitySumM_t    eqiSumM;
    Pcase EquityIndexQuoteWithSize:    QuoteM_t        eqiQuoteM; 

    Pcase Administrative:      Pstring_SE(:ETXorUS:)   adminM;
    Pcase FCOEndOfDaySummary:  FCOSumM_t               fcoSumM;
    Pcase Control:             Pstring_SE(:ETXorUS:)   controlM;
    Pcase FCOLastSale:         FCOLastSaleM_t          fcoSaleM;
    Pcase FCOQuote:            FCOQuoteM_t             fcoQuoteM;
    Pcase UnderlyingValue:     ValueM_t(:h.mid.mtype.val.uvalueTy:) 
                                                       uvalueM;
    Pdefault:                  Pstring_SE(:ETXorUS:)   otherM; // accept extensions
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

