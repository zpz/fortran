# I have settled with a simple scheme:
#
#   Every directory that contains a Fortran module, along with a
#   Makefile, will contain one module only.
#   There may be other source files and header files, but they will all
#   by 'include'd into the module source.
#   Therefore in appearance the module source file (.f or .f90) is the
#   only file that gets compiled.
#   After compilation, a library archive will be created out of it.
#
#   Usually the module will default to 'public' its subroutines.
#   Additional source files may have a corresponding header file (.h),
#   which define interfaces and set certain members to 'private'.
#   These header files will be 'include'd in the module preamble.
#   The source files will be 'include'd int he module's 'contains'
#   section.

# The following 2 variables should be set by the caller.
libname ?=
libsrc ?=

# Run 'make' with command-line option 'precision=single' or 'precision=double'.
# Without it, 'double' is assumed.
precision ?= double

# 'libsrc' is always a module.
# 'libsrc' is not required to match 'libname', although they usually match.
# 'libarc' is the lib archive.

libobj = $(pref)$(addsuffix .o,$(basename $(libsrc))$(suff))
libsrcmod = $(addsuffix .mod,$(basename $(libsrc)))
libmod = $(pref)$(libname)$(suff).mod
libarc = lib$(pref)$(libname)$(suff).a

define contingent-vars
  pref :=
  suff := $(if $(filter single,$1),_single,$(if $(filter double,$1),_double,))
  fflocal := $(FFLAGS)
  fflocal += $(if $(filter single,$1),,$(if $(filter double,$1),-r8,))
endef

double = $(eval $(call contingent-vars,double))
single = $(eval $(call contingent-vars,single))


# A little trick to make it compile for both double and single precisions.
# Because of this trick, the real targets are called
#   mybuild, myinstall, ...

define domake
    make my$1 precision=double; \
    make my$1 precision=single
endef

build:
	@$(call domake,$@)

clean:
	@$(call domake,$@)

install:
	@$(call domake,$@)

uninstall:
	@$(call domake,$@)

# End of trick.
#-------------#

mybuild: $($(precision)) $(libobj)
	$(AR) $(ARFLAGS) $(libarc) $(libobj)
	$(RANLIB) $(libarc)


myinstall: build
	$(INSTALL) $(libarc) $(LIBDIR)
  ifneq '$(strip $(libmod))' ''
	$(INSTALL) $(libmod) $(INCLUDEDIR)
  endif


myclean: $($(precision))
	rm -f $(libobj) $(libmod) $(libarc)


myuninstall: $($(precision))
	-$(UNINSTALL) $(LIBDIR)/$(libarc)
  ifneq '$(strip $(libmod))' ''
	-$(UNINSTALL) $(INCLUDEDIR)/$(libmod)
  endif


$(pref)%$(suff).o: %.f90
	$(FC) $(fflocal) -c $< -o $@
	mv -f $(libsrcmod) $(libmod)

$(pref)%$(suff).o: %.f
	$(FC) $(fflocal) -c $< -o $@
	mv -f $(libsrcmod) $(libmod)


