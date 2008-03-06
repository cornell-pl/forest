#include <regex.h>
#include <stdio.h>

/*
struct pos {
    int s;
    int l;
};

struct pos match (char *string, char *pattern)
{
    int status;
    regex_t re;
    regmatch_t pm;
    struct pos ret;

    ret.s = -1;
    ret.l = -1;

    if(regcomp(&re, pattern, REG_EXTENDED) != 0) return ret;

    status = regexec (&re, &string[0], 1, &pm, 0);

    printf ("pm.so = %d, pm.eo = %d\n", pm.rm_so, pm.rm_eo);


    pre = string;
    while (status==0) {
        printf ("find a match, pm.so = %d, pm.eo = %d\n", pm.rm_so, pm.rm_eo);
        status = regexec(&re, pre+pm.rm_eo, 1, &pm, REG_NOTBOL);
        pre = pre+pm.rm_eo;
    }

    if (status==0) {
        ret.s = pm.rm_so;
        ret.l = pm.rm_eo - pm.rm_so;
    }

    regfree(&re);
    return ret;
}

struct pos global;
*/

int get_start(char *string, char *pattern)
{
/*
    global = match(string, pattern);
    return global.s;
*/
    int status;
    regex_t re;
    regmatch_t pm;
    int s;

    if(regcomp(&re, pattern, REG_EXTENDED) != 0) return -2;
    status = regexec (&re, &string[0], 1, &pm, 0);

    if (status==0) {s = pm.rm_so;} else {s = -1;}

    regfree(&re);

    return s;
}

int get_len(char *string, char *pattern)
{
//    return global.l;
    int status;
    regex_t re;
    regmatch_t pm;
    int l;

    if(regcomp(&re, pattern, REG_EXTENDED) != 0) return -2;
    status = regexec (&re, &string[0], 1, &pm, 0);

    if (status==0) {l = pm.rm_eo - pm.rm_so;} else {l = -1;}

    regfree(&re);

    return l;

}
/*
int myprint (char* s1, char* s2)
{
    printf_int_int("%s, %s\n", s1, s2);
    return 1;
}
*/

