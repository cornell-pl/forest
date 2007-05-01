/* 
 * BGP (MRT-formatted) Update Messages 
 *
 * NB: This description covers a subset of the possible MRT messages namely
 * the BGP4MP type and only then the BGP UPDATE MESSAGE.
 *
 * Oh boy.  Start with http://tools.ietf.org/html/draft-ietf-grow-mrt-04 for
 * what it is worth.  See also http://tools.ietf.org/html/rfc4271
 * Section numbers throughout this file refer to these documents (Draft & RFC
 * respectively).
 *
 * + Psbh_uint*(:n:) is used to denote network byte ordered words.
 * + bit numbering is from high order bit per RFCs (i.e. b0 is MSB)
 *
 */

/* 
 * Draft-Sections 4 and 5.
 * You could be forgiven for thinking that we want BGP = 5, but in fact that
 * is deprecated and we really want BGP4MP = 16 ... 
 */
Penum MRT_Routing_Information_Type {
	BGP = 5,
	RIP = 6,
	IDRP = 7,	
        RIPNG = 8,
        BGP4PLUS = 9,
        BGP4PLUS_01 = 10,
        OSPF = 11,
        TABLE_DUMP = 12,
        TABLE_DUMP_V2 = 13,
        BGP4MP = 16,		/- this is the one of interest
        BGP4MP_ET = 17,
        ISIS = 32,
        ISIS_ET = 33,
        OSPFv3 = 48,
        OSPFv3_ET = 49
};

/*
 * Draft-Section 5.9
 */
Penum BGP4MP_Subtype {
       BGP4MP_STATE_CHANGE = 0,
       BGP4MP_MESSAGE = 1,	/- this is the one of interest
       BGP4MP_ENTRY = 2,
       BGP4MP_SNAPSHOT = 3,
       BGP4MP_STATE_CHANGE_AS4 = 4,
       BGP4MP_MESSAGE_AS4 = 5
};

/*
 * RFC-Section 4.1
 */
Penum BGP_Message_Type {
	BGP_OPEN_MSG = 1,
	BGP_UPDATE_MSG = 2,
	BGP_NOTIFICATION_MSG = 3,
	BGP_KEEPALIVE_MSG = 4,
	BGP_ROUTE_REFRESH_MSG = 5	/- RFC 2918
};

/*
 * RFC-Section 4.3: BGP UPDATE Message Format, Withdrawn Routes
 *
 *    Withdrawn Routes:
 *
 *       This is a variable-length field that contains a list of IP
 *       address prefixes for the routes that are being withdrawn from
 *       service.  Each IP address prefix is encoded as a 2-tuple of the
 *       form <length, prefix>, whose fields are described below:
 *
 *                +---------------------------+
 *                |   Length (1 octet)        |
 *                +---------------------------+
 *                |   Prefix (variable)       |
 *                +---------------------------+
 *
 *          The Length field indicates the length in bits of the IP
 *          address prefix.  A length of zero indicates a prefix that
 *          matches all IP addresses (with prefix, itself, of zero
 *          octets).
 *
 *          The Prefix field contains an IP address prefix, followed by
 *          the minimum number of trailing bits needed to make the end
 *          of the field fall on an octet boundary.  Note that the value
 *          of trailing bits is irrelevant.
 */
Pstruct BGP_WithdrawnRoute_t {
	Psbh_uint8(:1:)	IPaddrPrefixLenInBits;
	Pcompute Puint32 IPaddrPrefixLenInOctets = (IPaddrPrefixLenInBits + 7) / 8;
	Psbh_uint8(:1:) [IPaddrPrefixLenInOctets] IPaddrPrefix;
};

/*
 * RFC-Section 4.3: BGP UPDATE Message Format, Withdrawn Routes
 *
 *	...
 *    +-----------------------------------------------------+
 *    |   Withdrawn Routes (variable)                       |
 *    +-----------------------------------------------------+
 *	...
 *
 * BGP_WithdrawnRoutes_t[] is embedded in one branch of a union
 * (BGP_WithdrawnRoutes_u) in order to handle the case where withdrawRoutesLen
 * is 0 (i.e. no withdrawn routes are present in the message).
 *
 * TODO: reinvestigate putting withdrawn routes[] in the Pdefault case
 * directly.
 */
Parray BGP_WithdrawnRoutes_t(:size_t withdrawnRoutesLen:) {
	/* 
	 * PADS Issue: Documentation describes an array variable called "end" 
	 * but it doesn't seem to exist.
	 */
	BGP_WithdrawnRoute_t [] 
		: Plast(Pparsecheck((eltEnd.offset - begin.offset) >= withdrawnRoutesLen));
};

Punion BGP_WithdrawnRoutes_u(:Puint32 withdrawnRoutesLen:) {
	Pswitch (withdrawnRoutesLen) {
		Pcase 0  : Pcompute Puint32		noWithdrawnRoutes = 1;
		Pdefault : BGP_WithdrawnRoutes_t(:withdrawnRoutesLen:)	withdrawnRoutes;
	}
};

/*
 * RFC-Section 4.3: UPDATE Message Format, BGP Path Attribute Type
 *
 * Path Attribute Type is further decomposed into Flags and Type Code.
 *
 *             0                   1
 *             0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
 *             +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *             |  Attr. Flags  |Attr. Type Code|
 *             +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 */
Pstruct BGP_PathAttributeType_t {
	/*
	 * First the Attribute Flags: 
	 */
	Psbh_uint8(:1:)	pathAttributeFlags;

	/*
	 * b0 - Optional	: 0 = "well-known", 1 = optional
	 * b1 - Transitive	: 0 = non-transitive, 1 = transitive
	 * b2 - Partial		: 0 = complete, 1 = partial
	 * b3 - Extended Length : 0 = len 1 octect, 1 = len 2 octects
	 * b4..b7 - unused	: unused
	 *
	 * Rules:
	 * !b0 -> b1		: well-known attrs must be transitive
	 * !b0 -> !b2		: well-known attrs must be complete
	 *  b0 & !b1 -> !b2	: optional, non-transitive attrs must be complete
	 *  b4..b7 == 0		: unused
	 */
	Pcompute Puint8 b0_optional	= ((pathAttributeFlags & 0x80) >> 7);
	Pcompute Puint8 b1_transitive	= ((pathAttributeFlags & 0x40) >> 6)
		: ((!b0_optional && b1_transitive) || (b0_optional));
	Pcompute Puint8 b2_partial	= ((pathAttributeFlags & 0x20) >> 5)
		: (
			((!b0_optional && !b2_partial) || (b0_optional)) &&
			((b0_optional && !b1_transitive && !b2_partial) || (!(b0_optional && !b1_transitive)))
		  );
	Pcompute Puint8 b3_extended	= ((pathAttributeFlags & 0x10) >> 4);
	Pcompute Puint8 b4_b7		= ((pathAttributeFlags & 0x0F))
		: (b4_b7 == 0);

	/*
	 * And finally, the Attribute Type Code. See RFC-Section 5.
	 */
	Psbh_uint8(:1:)	pathAttributeTypeCode;
};

/*
 * RFC-Section 4.3: UPDATE Message Format, BGP Path Attribute Type, Length
 *
 */
Punion BGP_PathAttributeLength_u(:int extended:) {
	Pswitch (extended) {
		Pcase 0  : Psbh_uint8(:1:)	length;
		Pcase 1  : Psbh_uint16(:2:)	extendedLength;
		Pdefault : Pchar x : 0;	/- force an error
	}
};

/*
 * RFC-Section 4.3: UPDATE Message Format, BGP Path Attribute Type, Value
 *
 * TODO: More detail could be provided here.  While this element may be
 * entirely opaque since we have its length, some application might find
 * it useful to have the data described and (additional) constraints placed 
 * upon it.
 */
Parray BGP_PathAttributeValue_t(:size_t attrValLen:) {
	Psbh_uint8(:1:) [attrValLen];
};

/*
 * Each BGP Path Attribute is a 3-tuple of  <type, length, value> where 
 * type is itself made up of flags and a type code.
 */
Pstruct BGP_PathAttribute_t {
	BGP_PathAttributeType_t	pathAttributeType;
	/* 
	 * b3 of the Path Attribute Flags (contained in the Path Attribute
	 * Type) indicates if the Attribute Length is contained in one octect
	 * (clear) or two (set).
	 */
	Pcompute Puint8 extended = (pathAttributeType.pathAttributeFlags & 0x10) >> 4;
	BGP_PathAttributeLength_u(:extended:)	pathAttributeLength;
	/* 
	 *
	 */
	Pcompute size_t attrLength = (extended ? pathAttributeLength.val.extendedLength : pathAttributeLength.val.length);
	BGP_PathAttributeValue_t(:attrLength:) pathAttributeValue;
};

/*
 * RFC-Section 4.3: UPDATE Message Format, Path Attributes
 *
 * A variable length sequence of 3-tuples <type, length, value> where
 * type is itself made up of flags and a type code.
 *
 * BGP_PathAttributes_t[] is embedded in one branch of a union
 * (BGP_PathAttributes_u) in order to handle the case where
 * totalPathAttributesLength is 0 (i.e. no Path Attributes are present in the
 * message).
 */
Parray BGP_PathAttributes_t(:size_t totalPathAttributesLength:) {
	/* 
	 * PADS Issue: Documentation describes an array variable called "end" 
	 * but it doesn't seem to exist.
	 */
	BGP_PathAttribute_t [] 
		: Plast(Pparsecheck((eltEnd.offset - begin.offset) >= totalPathAttributesLength));
};

Punion BGP_PathAttributes_u(:Puint32 totalPathAttributesLength:) {
	Pswitch (totalPathAttributesLength) {
		Pcase 0  : Pcompute Puint32		noPathAttributes = 1;
		Pdefault : BGP_PathAttributes_t(:totalPathAttributesLength:)	pathAttributes;
	}
};

/*
 * RFC-Section 4.3: BGP UPDATE Message Format, Network Layer Reachability Info
 *
 *    Network Layer Reachability Information:
 *
 *       This variable length field contains a list of IP address
 *       prefixes.  (See comments for enclosing type on computing length of
 *       this section.)
 *
 *       Reachability information is encoded as one or more 2-tuples of
 *       the form <length, prefix>, whose fields are described below:
 *       (Same format as BGP_WithdrawnRoute_t)
 *
 *                +---------------------------+
 *                |   Length (1 octet)        |
 *                +---------------------------+
 *                |   Prefix (variable)       |
 *                +---------------------------+
 *
 *          The Length field indicates the length in bits of the IP
 *          address prefix.  A length of zero indicates a prefix that
 *          matches all IP addresses (with prefix, itself, of zero
 *          octets).
 *
 *          The Prefix field contains an IP address prefix, followed by
 *          enough trailing bits to make the end of the field fall on an
 *          octet boundary.  Note that the value of the trailing bits is
 *          irrelevant.
 */
Pstruct BGP_NetworkLayerReachabilityInfo_t {
	Psbh_uint8(:1:)	IPaddrPrefixLenInBits;
	Pcompute Puint32 IPaddrPrefixLenInOctets = (IPaddrPrefixLenInBits + 7) / 8;
	Psbh_uint8(:1:) [IPaddrPrefixLenInOctets] IPaddrPrefix;
};

/*
 * RFC-Section 4.3: BGP UPDATE Message Format, Network Layer Reachability Info
 *
 *	...
 *    +-----------------------------------------------------+
 *    |   Network Layer Reachability Information (variable) |
 *    +-----------------------------------------------------+
 *	...
 *
 * BGP_NetworkLayerReachabilityInfo_t[] is embedded in one branch of a union
 * (BGP_NetworkLayerReachabilityInfo_u) in order to handle the case where 
 * networkLayerReachabilityInfoLen is 0 (i.e. no withdrawn routes are present in 
 * the message).
 */
Parray BGP_NetworkLayerReachabilityInfos_t(:size_t networkLayerReachabilityInfoLen:) {
	/* 
	 * PADS Issue: Documentation describes an array variable called "end" 
	 * but it doesn't seem to exist.
	 */
	BGP_NetworkLayerReachabilityInfo_t [] 
		: Plast(Pparsecheck((eltEnd.offset - begin.offset) >= networkLayerReachabilityInfoLen));
};

Punion BGP_NetworkLayerReachabilityInfo_u(:Puint32 networkLayerReachabilityInfoLen:) {
	Pswitch (networkLayerReachabilityInfoLen) {
		Pcase 0  : Pcompute Puint32 noNetworkLayerReachabilityInfo = 1;
		Pdefault : BGP_NetworkLayerReachabilityInfos_t(:networkLayerReachabilityInfoLen:)	networkLayerReachabilityInfo;
	}
};

/*
 * RFC-Section 4.3: BGP UPDATE Message Format
 *
 *    +-----------------------------------------------------+
 *    |   Withdrawn Routes Length (2 octets)                |
 *    +-----------------------------------------------------+
 *    |   Withdrawn Routes (variable)                       |
 *    +-----------------------------------------------------+
 *    |   Total Path Attribute Length (2 octets)            |
 *    +-----------------------------------------------------+
 *    |   Path Attributes (variable)                        |
 *    +-----------------------------------------------------+
 *    |   Network Layer Reachability Information (variable) |
 *    +-----------------------------------------------------+
 *
 * The Network Layer Reachability Information length is computed according
 * to the RFC as 
 * 
 * 	UPDATE message Length - 23 - Total Path Attributes Length 
 * 	- Withdrawn Routes Length.  
 *
 * Here, the parameter BGPUpdateContentsLen already accounts for the header 
 * length (19 octets) so the computation below uses 4 instead of 23.
 */
Pstruct BGP_Update_t(:size_t BGPUpdateContentsLen:) {
	Psbh_uint16(:2:)				withdrawnRoutesLen;	
	BGP_WithdrawnRoutes_u(:withdrawnRoutesLen:)	withdrawnRoutes;

	Psbh_uint16(:2:)				totalPathAttributeLen;	
	BGP_PathAttributes_u(:totalPathAttributeLen:)	pathAttributes;

	/* 
	 * Network Reachability Layer Info length is not explicitly in the
	 * message but is easily computed:
	 */
	Pcompute size_t networkLayerReachabilityInfoLen = 
		BGPUpdateContentsLen - 4 - totalPathAttributeLen - withdrawnRoutesLen;
	BGP_NetworkLayerReachabilityInfo_u(:networkLayerReachabilityInfoLen:)	networkLayerReachabilityInfo;
};

/*
 * RFC-Section 4.1 BGP Message Header Format
 *
 *    0                   1                   2                   3
 *    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
 *    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *    |                                                               |
 *    +                                                               +
 *    |                                                               |
 *    +                                                               +
 *    |                           Marker                              |
 *    +                                                               +
 *    |                                                               |
 *    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *    |          Length               |      Type     |
 *    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 */
Parray BGP_Msg_Marker_t {
 	/* 
	 * The BGP Message Header begins with a marker of 16 octets with 
	 * all bits set to 1.
	 */
	Psbh_uint8(:1:) [16];
} Pwhere {
	Pforall( i Pin [0..length-1] : elts[i] == 0xFF );
};

Pstruct BGP_Message_Header_t {
	BGP_Msg_Marker_t	BGPMsgMarker;
	/*
	 * BGP Messages have length constraints of 19..4096 octets including
	 * the header itself.
	 */
	Psbh_uint16(:2:)	BGPMsgLen
		: ((BGPMsgLen >= 19) && (BGPMsgLen <= 4096));
	/*
	 * We're only interested in/capable of dealing with BGP UPDATE messages 
	 * so constrain parsing.
	 */
	Psbh_uint8(:1:)		BGPMsgType
		: (BGPMsgType == BGP_UPDATE_MSG);
};

/*
 * RFC-Section 4. BGP Message Formats
 *
 * See BGP_Message_Header_t above.
 * 
 * Also, in the Draft, see "BGP Message", as described in BGP4MP_Message_t.  
 * (Said to be similar to a BGP_UPDATE Message)
 */
Pstruct BGP_Message_t(:size_t BGPMsgLen:) {
	/*
	 * The container imposes a length on the message (BGPMsgLen)
	 * and the header contains a length (BGPMsgHdr.BGPMsgLen).
	 * Might as well verify they match.
	 */
	BGP_Message_Header_t	BGPMsgHdr	
		: (BGPMsgHdr.BGPMsgLen == BGPMsgLen);
	/*
	 * The variant part of the message has a length that is the overall 
	 * BGP message's less 19 octects for the header.
	 */
	Pcompute size_t BGPUpdateMsgLen = BGPMsgHdr.BGPMsgLen - 19;
	BGP_Update_t(:BGPUpdateMsgLen:)	BGPUpdate;
};

/* 
 * Draft-Section 5.9.2: BGP4MP_MESSAGE Subtype
 *
 *      0                   1                   2                   3
 *      0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
 *     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *     |        Source AS number       |     Destination AS number     |
 *     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *     |        Interface Index        |        Address Family         |
 *     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *     |                     Source IP address (variable)              |
 *     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *     |                   Destination IP address (variable)           |
 *     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *     |                    BGP Message... (variable)
 *     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 * This is the last type derived mainly from the Draft document, the next
 * interesting type (BGP_Message_t) is derived mainly from the RFC.
 */
Pstruct BGP4MP_Message_t(:size_t BGP4MPMsgLen:) {
	Psbh_uint16(:2:)	sourceASNumber;
	Psbh_uint16(:2:)	destinationASNumber;
	Psbh_uint16(:2:)	interfaceIndex;
	/*
	 * Address Family: 0=unknown/unsupported, 1=AFI_IPv4, 2=AFI_IPv6
	 * Assume IPv4 for now and provide constraint.
	 */
	Psbh_uint16(:2:)	addressFamily	: (addressFamily == 0x01);	
	Psbh_uint32(:4:)	sourceIPAddress;	/- type assumes IPv4
	Psbh_uint32(:4:)	destinationIPAddress;	/- type assumes IPv4
	/* 
	 * BGP4MPMsgLen is available from MRT Message but not necessarily 
	 * required by the BGPMsg.
	 *
	 * TODO: Use BGP4MPMsgLen and its ilk as an additional parsing check.
	 */
	Pcompute size_t BGPMsgLen = BGP4MPMsgLen - 16;	/- available but not req'd
	BGP_Message_t(:BGPMsgLen:)	BGPMsg;		/- switch from Draft (MRT) to RFC (BGP)
};

/* 
 * Draft-Section 3. The MRT header. Common to all MRT records.
 *
 *      0                   1                   2                   3
 *      0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
 *     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *     |                           Timestamp                           |
 *     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *     |             Type              |            Subtype            |
 *     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *     |                             Length                            |
 *     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *     |                      Message... (variable)
 *     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 * We supply constraints to restrict parsing to the case where the message
 * type is BGP4MP and the message subtype is BGP4MP_MESSAGE.
 */
Pstruct MRT_header_t
{
	Psbh_uint32(:4:)	MRTMsgTimeStamp;
	/* 
	 * Restrict processing based on this description to BGP4MP/BGP4MP_MESSAGE 
 	 * messages only. 
	 *
	 * TODO: How to get a multi-byte enum in with proper network byte
	 * ordering?
	 */
	Psbh_uint16(:2:)	MRTMsgType 	: (MRTMsgType == BGP4MP); 
	Psbh_uint16(:2:)        MRTMsgSubtype	: (MRTMsgSubtype == BGP4MP_MESSAGE);
	Psbh_uint32(:4:)	MRTMsgLen;	/- Exclusive of self + hdr
};

/* 
 * Draft-Section 3. The MRT Message (see header above).
 *
 * A degenerate MRT Record containing only the BGP4MP_Message_t case 
 * (MRTMsgHdr.MRTMsgType = 16, MRTMsgHdr.MRTMsgSubtype = 1)
 */
Pstruct MRT_Message_t {
	MRT_header_t				MRTMsgHdr;		
	BGP4MP_Message_t(:MRTMsgHdr.MRTMsgLen:)	BGP4MPMsg;	/- Only case we're interested in
};

Parray BGP_MRT_Updates {
	MRT_Message_t [];
};
