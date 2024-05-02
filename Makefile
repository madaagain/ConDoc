##
## EPITECH PROJECT, 2024
## B-FUN-400-PAR-4-1-wolfram-alexis.mer
## File description:
## Makefile
##

BINARY_PATH := $(shell stack path --local-install-root)

NAME = mypandoc

all: $(NAME)

build:
	stack build

$(NAME): build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)
	rm -f $(BINARY_PATH)/bin/$(BINARY_NAME)-exe

re: fclean all

.PHONY: all clean fclean re
