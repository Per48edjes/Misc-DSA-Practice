CC := gcc

CFLAGS  = -std=c99
CFLAGS += -g
CFLAGS += -Wall
CFLAGS += -Wextra
CFLAGS += -pedantic
CFLAGS += -Werror
CFLAGS += -Wmissing-declarations
CFLAGS += -DUNITY_SUPPORT_64 -DUNITY_OUTPUT_COLOR

ASANFLAGS  = -fsanitize=address
ASANFLAGS += -fno-common
ASANFLAGS += -fno-omit-frame-pointer

BUILD_DIR := build
ENV_NAME := avl_tree_env
WORKING_DIR := app

.PHONY: build
build:
	docker build -t $(ENV_NAME) .
	mkdir -p $(BUILD_DIR)/

.PHONY: run
run:
	docker run --rm -it -p 8080:8080 -v $(PWD):/$(WORKING_DIR) $(ENV_NAME)

.PHONY: clean
clean:
	@echo Cleaning $(BUILD_DIR)/
	rm -rf $(BUILD_DIR)/*

.PHONY: test
test: tests.out
	@$(BUILD_DIR)/tests.out

.PHONY: memcheck
memcheck: ./*.c ./*.h
	@echo Compiling $@
	@$(CC) $(ASANFLAGS) $(CFLAGS) test-framework/unity.c ./*.c -o $(BUILD_DIR)/memcheck.out
	@$(BUILD_DIR)/memcheck.out
	@echo "Memory check passed"

tests.out: ./*.c ./*.h
	@echo Compiling $@
	@$(CC) $(CFLAGS) test-framework/unity.c ./*.c -o $(BUILD_DIR)/tests.out
