#include "vanilla.p"
Pstruct timestamp_t {
  Puint64 first;
  '.';
  Puint32 second;
}

Ptypedef Pstring(:' ':) hostname_t;

Pstruct trailer_t {
       timestamp_t ipc;  
  ' '; hostname_t  name;
  ' '; timestamp_t logger;  
};

Pstruct int_float_t{
       Puint32 i;
  ' '; Pfloat32 f;
}

Parray int_floats_t(:Puint32 size:) {
  int_float_t [size] : Psep(' ');
}

Parray floats_t(:Puint32 size:) {
  Pfloat32 [size] : Psep(' ');
}

Parray int32s_t(:Puint32 size:) {
  Pint32 [size] : Psep(' ');
}

Pstruct can2_t {
  ' '; floats_t(:3:) fs1;
  ' '; int32s_t(:3:) is1;
  ' '; floats_t(:8:) fs2;
  ' '; int32s_t(:4:) is2;
  ' '; floats_t(:4:) fs3;
  ' '; int32s_t(:6:) is3;
  ' '; Pfloat32 last_f;
}

Pstruct ekf_pose2_t {
  ' '; floats_t(:9:)  fs1;
  ' '; Pstring(:' ':) s;
  ' '; floats_t(:17:) fs2;
}

Pstruct heartbeat_t{
  ' '; Pstring(:' ':) which;
}

Pstruct imu_t {
  ' '; floats_t(:6:) fs;
}

Pstruct laser_t {
  ' '; Pfloat32 f1;
  ' '; Puint32  i1;
  ' '; floats_t(:181:) fs;
  ' '; Puint32  i2;
};

Pstruct param_t {
  ' '; Pstring(:' ':) name;
  ' '; Pstring(:' ':) value;
};

Pstruct cont_target_t {
  ' '; floats_t(:4:)  fs;
};

Pstruct touareg_act_t {
  ' '; floats_t(:3:)  fs;
};

Pstruct gps_pos5_t {
  ' '; int32s_t(:6:)  i1s;
  ' '; Pfloat32      f1;
  ' '; int32s_t(:4:)  i2s;
  ' '; floats_t(:7:) fs;
};

Pstruct gps_vel4_t {
  ' '; int32s_t(:6:)  i1s;
  ' '; Pfloat32       f1;
  ' '; Puint32        i1;
  ' '; floats_t(:4:)  fs;
};

Pstruct planner_info_t {
  ' '; Pfloat32          f1;
  ' '; Pstring(:' ':)    name;
  ' '; int_floats_t(:3:) ifs;
  ' '; floats_t(:7:)     fs;
}

Pstruct planner_traj2_t {
  ' '; Puint32  i1;
  ' '; floats_t(:625:) fs;
};

Pstruct gps_comp_attitude_t {
  ' '; Puint32        i1;
  ' '; Pfloat32       f1;
  ' '; Puint32        i2;
  ' '; floats_t(:6:)  fs;
  ' '; Puint32        i3;
};

Pstruct temp_t {
  ' '; Puint32        i1;
  ' '; Pfloat32       f1;
}

Penum tag_t {
  CAN2,
  EKF_POSE2,
  HEARTBEAT,
  IMU, 
  LASER1_2,  LASER2_2, LASER3_2, LASER4_2,  LASER5_2, 
  PARAM,  
  CONT_TARGET, 
  TOUAREG_ACT, 
  GPS_POS5, 
  GPS_VEL4, 
  PLANNER_INFO, 
  PLANNER_TRAJ2, 
  GPS_COMP_ATTITUDE, 
  TEMP
}

Punion vbody_t (: tag_t t :) {
  Pswitch (t) {
    Pcase CAN2      : can2_t             can2;
    Pcase EKF_POSE2 : ekf_pose2_t        ekf_pose2;
    Pcase HEARTBEAT : heartbeat_t        heartbeat;
    Pcase IMU       : imu_t              imu;
    Pcase LASER1_2  : laser_t            laser1;
    Pcase LASER2_2  : laser_t            laser2;
    Pcase LASER3_2  : laser_t            laser3;
    Pcase LASER4_2  : laser_t            laser4;
    Pcase LASER5_2  : laser_t            laser5;
    Pcase PARAM     : param_t            param;
    Pcase CONT_TARGET       : cont_target_t       cont_target;
    Pcase TOUAREG_ACT       : touareg_act_t       touareg_act;
    Pcase GPS_POS5          : gps_pos5_t          gps_pos5;
    Pcase GPS_VEL4          : gps_vel4_t          gps_vel4; 
    Pcase PLANNER_INFO      : planner_info_t      planner_info;
    Pcase PLANNER_TRAJ2     : planner_traj2_t     planner_traj2;
    Pcase GPS_COMP_ATTITUDE : gps_comp_attitude_t gps_comp_attitude; 
    Pcase TEMP              : temp_t              temp;
  }
}

Precord Pstruct entry_t {
       tag_t tag;
       vbody_t(:tag:) vbody;
  ' '; trailer_t trailer;
}

Precord Pstruct header_entry_t {
  "# ";
  Pstring_SE(:Peor:) comment;
}

Parray header_t {
  header_entry_t[] : Plongest;
}

Pstruct log_t {
  header_t header;
  entry_t[] entries;
}
