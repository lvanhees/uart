RTLDIR=rtl

GHDL=ghdl
GHDLFLAGS=--std=93c --workdir=$(RTLDIR) --ieee=standard
GHDLRUNFLAGS=

# Default target : elaborate
all: anal elab run

# Elaborate target.  Almost useless
elab: force
		$(GHDL) -c $(GHDLFLAGS) -e uart

# Run target
run: force
		$(GHDL) -c $(GHDLFLAGS) -r uart $(GHDLRUNFLAGS)

# Targets to analyze libraries
anal: force
		$(GHDL) -a $(GHDLFLAGS) $(RTLDIR)/uart_baudgen.vhd
		$(GHDL) -a $(GHDLFLAGS) $(RTLDIR)/uart_rx.vhd
		$(GHDL) -a $(GHDLFLAGS) $(RTLDIR)/uart.vhd

force:
