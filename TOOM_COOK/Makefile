CC = gcc
//CFLAGS = -I include -Wall -Wextra -O0 -g -std=c99
CFLAGS = -I include -Wall -Wextra -O3 -std=c99
SRC = src/schoolbook.c \
src/toomcook44.c src/toomcook416.c src/kernel.c src/toomcook464.c src/toomcook4.c src/toomcook4_pipeline.c\
src/calculator.c src/test.c \
app/main.c
OBJ = $(SRC:.c=.o)
TARGET = a.out

all: $(TARGET)

$(TARGET): $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ) $(TARGET)

test: $(TARGET)
	./$(TARGET)
	rm -f $(OBJ) $(TARGET)
.PHONY: all clean test