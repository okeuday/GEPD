#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

INTERFACE_HEADER = erlang_functions.hrl
CURRENT_VERSION = vsn_1

instdir = $(prefix)/lib/GEPD/
inst_PROGRAMS = test_functions_port_vsn_1
inst_LTLIBRARIES = test_functions_port_driver_vsn_1.la
beamdir = "$(instdir)"
beam_DATA = test_bindings.beam

BUILT_SOURCES = $(INTERFACE_HEADER) $(beam_DATA)
CLEANFILES = $(INTERFACE_HEADER) $(beam_DATA)

$(INTERFACE_HEADER): Makefile \
                     erlang_functions_hrl.h
	$(CXX) -DCURRENT_VERSION=$(CURRENT_VERSION) \
         -include $(srcdir)/test_bindings.h \
         $(BOOST_CPPFLAGS) -E -P $(srcdir)/erlang_functions_hrl.h > $@

test_functions_port_vsn_1_SOURCES = test_functions.c \
                                    main.cpp \
                                    port.cpp
test_functions_port_vsn_1_CPPFLAGS = \
 -I$(ERLANG_LIB_DIR_erl_interface)/include/ \
 -I$(ERLANG_ROOT_DIR)/erts-$(ERLANG_ERTS_VER)/include/ \
 -DCURRENT_VERSION=$(CURRENT_VERSION) $(BOOST_CPPFLAGS) \
 -include $(srcdir)/test_bindings.h $(CXXFLAGS) -save-temps
test_functions_port_vsn_1_LDADD = -lei
test_functions_port_vsn_1_LDFLAGS = -L$(ERLANG_LIB_DIR_erl_interface)/lib/

test_functions_port_driver_vsn_1_la_SOURCES = test_functions.c \
                                              port_driver.cpp 
test_functions_port_driver_vsn_1_la_CPPFLAGS = \
 -fPIC -I$(ERLANG_ROOT_DIR)/erts-$(ERLANG_ERTS_VER)/include/ \
 -DCURRENT_VERSION=$(CURRENT_VERSION) $(BOOST_CPPFLAGS) \
 -include $(srcdir)/test_bindings.h $(CXXFLAGS) \
 -Wno-unused-function -save-temps
test_functions_port_driver_vsn_1_la_LDFLAGS = -module -avoid-version

.erl.beam:
	@ERLC@ -b beam $(ERLC_OPTS) -o $@ $<

install-exec-hook: $(beam_DATA)
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) test_bindings.beam $(instdir)

