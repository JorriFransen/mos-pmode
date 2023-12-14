ASM=nasm

SRC_DIR=src
BUILD_DIR=build

image: $(BUILD_DIR)/floppy.img

$(BUILD_DIR)/floppy.img: $(BUILD_DIR)/boot.bin always
	cp $< $@
	truncate -s 1440k $@

$(BUILD_DIR)/boot.bin: $(SRC_DIR)/main.asm always
	$(ASM) $< -f bin -o $@

run: $(BUILD_DIR)/floppy.img
	qemu-system-i386 -fda $^

debug: $(BUILD_DIR)/floppy.img
	bochs -q -f bochs_config


always:
	@mkdir -p $(BUILD_DIR)

.PHONY: image run
