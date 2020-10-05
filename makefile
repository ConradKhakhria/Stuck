HC=ghc
RM=rm
MV=mv
CD=cd
CP=cp

INCLUDE=Parsing/Parsing
TARGET=stuck

all: $(TARGET)

$(TARGET):
	$(CP) Parsing/Parsing.hs src; $(CD) src; $(HC) $(TARGET); $(MV) $(TARGET) ..

clean:
	$(CD) src; $(RM) *.o; $(RM) *.hi; $(RM) ../$(TARGET); $(RM) Parsing.hs