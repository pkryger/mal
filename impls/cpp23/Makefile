BUILD_DIR=build
CMAKE=cmake
NINJA=ninja

MAINS=$(wildcard step*.cpp)
TARGETS=$(MAINS:%.cpp=$(BUILD_DIR)/%)
BUILD_NINJA=$(BUILD_DIR)/build.ninja

.PHONY: all cmake $(TARGETS) clean

$(BUILD_NINJA): CMakeLists.txt
	$(CMAKE) -S . -B $(BUILD_DIR) -G Ninja

all: $(BUILD_NINJA)
	$(NINJA) -C $(BUILD_DIR)

$(TARGETS): $(BUILD_NINJA)
	$(NINJA) -C $(BUILD_DIR) $(@:$(BUILD_DIR)/%=%)

clean:
	rm -rf $(BUILD_DIR)
