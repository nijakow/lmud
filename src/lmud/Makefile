EXECUTABLE = ./lmud.exe

INSTALL_DIR = /usr/local/bin

SOURCES = $(shell find . -name "*.c")
HEADERS = $(shell find . -name "*.h")

OBJECTS = $(patsubst %.c, %.o, $(SOURCES))
DEPENDS = $(patsubst %.c, %.d, $(SOURCES))

CFLAGS = -I.. -g -O3 -Wall -Wextra
LDFLAGS = 

.default: all
all: $(EXECUTABLE)

%.o: %.c
	$(CC) $(CFLAGS) -MMD -MP -c $< -o $@

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(OBJECTS) $(LDFLAGS) -o $(EXECUTABLE)

.phony: install
install: all
	cp $(EXECUTABLE) $(INSTALL_DIR)

.phony: run
run: all
	$(EXECUTABLE) $(LIME_IMAGE)

.phony: debug
debug: all
		gdb $(EXECUTABLE)

.phony: leaks
leaks: all
		valgrind --leak-check=full $(EXECUTABLE) $(LIME_IMAGE)

.phony: clean
clean:
	- $(RM) $(EXECUTABLE) $(OBJECTS) $(DEPENDS) $(LIME_IMAGE)

.phony: re
re: clean all

-include $(DEPENDS)
