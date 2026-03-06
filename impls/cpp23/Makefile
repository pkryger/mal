BUILD_DIR=build$(if $(CMAKE_BUILD_TYPE),/$(CMAKE_BUILD_TYPE))
CMAKE=cmake

NINJA=ninja
ifeq (, $(shell command -v $(NINJA) 2> /dev/null))
  GENERATED_FILE=$(BUILD_DIR)/Makefile
else
  GENERATED_FILE=$(BUILD_DIR)/build.ninja
  export CMAKE_GENERATOR=Ninja
endif

MAINS=$(wildcard step*.cpp)
TARGETS=$(MAINS:%.cpp=$(BUILD_DIR)/%)

.PHONY: all cmake $(TARGETS) clean

$(GENERATED_FILE): CMakeLists.txt
	$(CMAKE) -S . -B $(BUILD_DIR)

all: $(GENERATED_FILE)
	$(CMAKE) --build $(BUILD_DIR)

$(TARGETS): $(GENERATED_FILE)
	$(CMAKE) --build $(BUILD_DIR) --target $(@:$(BUILD_DIR)/%=%)

clean:
	rm -rf $(BUILD_DIR)
