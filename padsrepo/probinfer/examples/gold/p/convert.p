Precord Ptypedef Pstring_ME(:"/^\\-+$/":) line_delimiter;
Precord Ptypedef Pstring_ME(:"/\\-\\-\\-\\-\\w+/":) comments_t;
Precord Ptypedef Ptimestamp_explicit_SE(:"/$/", "%A, %B %d, %Y %H:%M:%S %p", P_cstr2timezone("-0500"):) datetime_t; 

Precord Pstruct status_t {
        "Status code ";
        Puint32 code;
        "creating thread ";
        Pstring_SE(:"/.$/":) threadname;
        '.';
};

Precord Pstruct begin_t {
        "Begin converting root volume ";
        Pstring(:'.':) volume;
        '.';
};

Precord Pstruct conv_prof_t {
        "Converted user profiles folders under ";
        Pstring(:'.':) volume;
        '.';
};

Precord Pstruct parse_t {
        "Parsing template ";
        Pstring_SE(:"/.$/":) template_path;
};
        
Precord Pstruct config_t {
        "Configure ";
        Pstring_SE(:"/.$/":) file_path;
        '.';
};

Precord Pstruct configed_t {
        "Configured ";
        Pstring_SE(:"/ with/":) volume;
        " with ";
        Pstring_SE(:"/.$/":) config_item;
        '.';
};

Precord Pstruct setting_sec_error_t {
        "Error setting security on ";
        Pstring_SE(:"/.$/":) filepath;
};

Precord Pstruct other_error_t{
        "Error ";
        Puint32 err_id;
        Pstring_SE(:"/.$/":) err_msg;
        '.';
};

Precord Punion error_t {
        setting_sec_error_t sec_setting_err;
        other_error_t other_err;
};

Precord Pstruct warning_t {
        "Warning ";
        Puint32 warning_id;
        ": The system cannot find the file specified.";
};

Precord Pstruct complete_t{
        Pstring_SE(:"/ was completed/":) action;
        " was completed sucessfully.";
};

Precord Ptypedef Pstring_ME(:"/^$/":) empty_line_t;

Precord Punion log_action_t {
        warning_t warn_msg;
        error_t error_msg;
        config_t config_msg;
};

Psource Pstruct log_t {
 line_delimiter delimit1;
 datetime_t ts_begin_log;
 status_t status;
 line_delimiter delimit2;
 datetime_t ts_conv_log;
 "Begin converting root volume ";
 Pstring_SE(:"/.$/":) volume;
 ":."; 
 Peor;
 conv_prof_t conv_prof_msg;
 parse_t parse_msg;
 comments_t conf_eng_comment;
 empty_line_t[] empty1;
 comments_t reading_comment;
 empty_line_t[] empty2;
 comments_t conf_sec_comment;
 log_action_t[] actions;
 empty_line_t[] empty3;
 complete_t completed_msg;
 empty_line_t[] empty4;
 comments_t uninit_comment;
 configed_t configed_msg1;
 log_action_t[] confactions;
 configed_t configed_msg2;
}; 
