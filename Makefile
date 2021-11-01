##
## EPITECH PROJECT, 2021
## HAL
## File description:
## Makefile
##

NAME		=	hal

all			:	$(NAME)

$(NAME)		:
				stack build
				mv $(shell stack path --local-install-root)/bin/$(NAME)-exe ./$(NAME)

clean		:
				$(RM) $(NAME)

fclean		:	clean
				stack clean

re			:	fclean all

tests_run	:
				stack build --test

.PHONY		:	all clean fclean re tests_run
