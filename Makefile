RTLDIR=rtl
SIMDIR=sim

STOPTIME=1500us

#VHDLSTD=93c
VHDLSTD=08

GHDL=ghdl
GHDLFLAGS=--std=$(VHDLSTD) --workdir=$(SIMDIR) --ieee=standard --warn-vital-generic
GHDLRUNFLAGS=--stop-time=$(STOPTIME) --wave=$(SIMDIR)/uart_tb.ghw

# Default target : elaborate
all: anal elab run

# Elaborate target.  Almost useless
elab:
		$(GHDL) -c $(GHDLFLAGS) -e uart_tb

# Run target
run:
		$(GHDL) -c $(GHDLFLAGS) -r uart_tb $(GHDLRUNFLAGS)

# Targets to analyze libraries
anal:
		$(GHDL) -a $(GHDLFLAGS) $(RTLDIR)/uart_baudgen.vhd
		$(GHDL) -a $(GHDLFLAGS) $(RTLDIR)/uart_rx.vhd
		$(GHDL) -a $(GHDLFLAGS) $(RTLDIR)/uart.vhd
		$(GHDL) -a $(GHDLFLAGS) $(SIMDIR)/uart_tb.vhd

view:
		gtkwave $(SIMDIR)/uart_tb.ghw $(SIMDIR)/view.gtkw	
clean:
