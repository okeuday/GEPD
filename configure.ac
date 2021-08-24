#-*-Mode:autoconf;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=config fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

AC_INIT([GEPD], [0.9.5], [mjtruog at protonmail dot com],
        [GEPD], [https://github.com/okeuday/GEPD])

# paths to check for installations that are not standard but typically used
# (for any libraries that don't depend on executables for paths)
PATHS_NONSYSTEM_INC=""
AC_CHECK_FILE("/opt/local/include",
              [PATHS_NONSYSTEM_INC="/opt/local/include $PATHS_NONSYSTEM_INC"])
AC_CHECK_FILE("/usr/local/include",
              [PATHS_NONSYSTEM_INC="/usr/local/include $PATHS_NONSYSTEM_INC"])
PATHS_NONSYSTEM_LIB=""
AC_CHECK_FILE("/opt/local/lib",
              [PATHS_NONSYSTEM_LIB="/opt/local/lib $PATHS_NONSYSTEM_LIB"])
AC_CHECK_FILE("/usr/local/lib",
              [PATHS_NONSYSTEM_LIB="/usr/local/lib $PATHS_NONSYSTEM_LIB"])

AC_PREREQ([2.59])
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([1.10 no-define no-dependencies foreign])
AC_PROG_MKDIR_P
AC_PROG_CC
AC_PROG_CXX
AC_PROG_SED
AC_PROG_LIBTOOL
AM_PROG_CC_C_O

# C++ requirements
AX_BOOST_BASE([1.40], ,
    [AC_MSG_ERROR([Boost >= 1.40 not found, see http://boost.org/])])
AX_BOOST_CHECK_HEADER(boost/preprocessor.hpp, ,
    [AC_MSG_ERROR([boost::preprocessor not found])], ,
    $PATHS_NONSYSTEM_INC)

# erlang checks
AC_ERLANG_NEED_ERLC()
AC_ERLANG_NEED_ERL()
AX_ERLANG_REQUIRE_OTP_VER([R14], ,
    [AC_MSG_ERROR([Erlang >= R12B01 not found])])
AC_ERLANG_SUBST_ERTS_VER
AC_ERLANG_SUBST_ROOT_DIR
AX_ERLANG_SUBST_OTP_VER
AC_ERLANG_SUBST_LIB_DIR
AC_ERLANG_CHECK_LIB([erl_interface])
AC_PATH_PROG([ESCRIPT],[escript], ,[`AS_DIRNAME([$ERLC])`])
AC_PATH_PROG([DIALYZER],[dialyzer], ,[`AS_DIRNAME([$ERLC])`])
AC_SUBST([ERLC_OPTS],
         ["-DERLANG_OTP_VERSION_${ERLANG_OTP_VER} -DERLANG_OTP_VERSION_${ERLANG_OTP_VER_MAJOR} -I $abs_top_srcdir/lib +debug_info +strict_validation +warn_bif_clash +warn_deprecated_function +warn_export_all +warn_export_vars +warn_exported_vars +warn_obsolete_guard +warn_shadow_vars +warn_unused_import +warn_unused_function +warn_unused_record +warn_unused_vars"])
if test "$ERLANG_OTP_VER_MAJOR" -ge 18 ; then
ERL_OPTS="+C multi_time_warp +c true"
else
ERL_OPTS=""
fi
AC_SUBST(ERL_OPTS)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

