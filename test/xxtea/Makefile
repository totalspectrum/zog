# Path to the gcc executable and friends
# TOOLPATH=/home/michael/ZPU/toolchain/install/bin/
TOOLPATH=/opt/zpu/bin/

# The compiler/linker
CC=$(TOOLPATH)zpu-elf-gcc

# objdump. Disassemble including line numbers and source code. 
#OD=$(TOOLPATH)zpu-elf-objdump -d -l --source
OD=$(TOOLPATH)zpu-elf-objdump -D -l --source

# objcopy. Just copy relevent sections to a binary image.
OC=$(TOOLPATH)zpu-elf-objcopy -O binary
#OC=$(TOOLPATH)zpu-elf-objcopy -O srec --srec-forceS3

# Compiler flags. Compile only, debug info, all warnings, optimize for size
CFLAGS=-c -g -Wall -Os -std=c99 

# The ZPU platform Phi board (phi) or Able board (able)
ZPUPLATFORM=-phi

#Linker flags. phi platform, shrink(relax) immediates, remove unwanted sections
LDFLAGS=$(ZPUPLATFORM) -Wl,--relax -Wl,--gc-sections

# Source files, add more here if you have
SOURCES=xxtea.c 

# Create a list of object file names from source file names
OBJECTS=$(SOURCES:.c=.o)

# The program to build 
EXECUTABLE=xxtea

# Binary output file name
BINARY=$(EXECUTABLE).bin

#Listing output file name
LISTING=$(EXECUTABLE).lst

# By default build an executeable, a raw binary RAM image and an assembler listing file 
all: $(SOURCES) $(EXECUTABLE) $(BINARY) $(LISTING)
	
# Link the executable from object files
$(EXECUTABLE): $(OBJECTS) 
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

# Convert elf executable to raw binary image
# And reverse endianness.
$(BINARY): $(EXECUTABLE)
	$(OC) $(EXECUTABLE) $(BINARY)
	objcopy -I binary -O binary --reverse-bytes=4  $(BINARY)

# Create a listing file
$(LISTING): $(EXECUTABLE)
	$(OD) $(EXECUTABLE) > $(LISTING)

# Compile .c files into objexts .o
.c.o:
	$(CC) $(CFLAGS) $< -o $@

# Clean up 
clean:
	rm -rf *.o *.lst *~ $(EXECUTABLE) $(BINARY) $(LISTING)
	
