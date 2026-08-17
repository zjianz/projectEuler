# ============================================================
# 通用 Haskell 编译配置
# ============================================================
GHC       = ghc
GHC_FLAGS = -O2 -Wall -package=arithmoi -package=array -package=containers -odir=tmp -hidir=tmp
SRC_DIR   = src
BUILD_DIR = build
OBJ_DIR   = tmp

# 自动发现所有 .hs 源文件（位于 $(SRC_DIR) 下）
SOURCES := $(wildcard $(SRC_DIR)/*.hs)
# 对应的目标可执行文件（放在 $(BUILD_DIR) 下，去掉扩展名）
TARGETS  := $(patsubst $(SRC_DIR)/%.hs,$(BUILD_DIR)/%,$(SOURCES))

# ============================================================
# 默认目标：编译所有可执行文件
# ============================================================
.PHONY: all clean

all: $(TARGETS)

# 模式规则：将 src/%.hs 编译为 build/%
$(BUILD_DIR)/%: $(SRC_DIR)/%.hs | $(BUILD_DIR) $(OBJ_DIR)
	$(GHC) $(GHC_FLAGS) -o $@ $<

# 保留模式规则产物（否则 make 会把 build/% 当作中间文件并在构建后删除）
.PRECIOUS: $(BUILD_DIR)/%

# 自动创建输出目录（仅当不存在时）
$(BUILD_DIR) $(OBJ_DIR):
	mkdir -p $@

# ============================================================
# 便捷规则：直接通过文件名编译（例如 make 023）
# ============================================================
# 让用户可以用 make 023 来代替 make build/023
# 注意：这必须放在 .PHONY 目标之后，以免覆盖 clean 等
%: $(BUILD_DIR)/%
	@:

# ============================================================
# 清理
# ============================================================
clean:
	rm -rf $(OBJ_DIR) $(BUILD_DIR)

# ============================================================
# init：从模板创建新题目并注册到 euler.cabal
# 用法：make init 037   或   make init N=037
# 编号为三位数字（如 037，与现有 001~036 命名一致）
# ============================================================
# 支持 make init 037 这种直接带编号的写法
ifeq ($(filter init,$(MAKECMDGOALS)),init)
ifndef N
N := $(word 2,$(MAKECMDGOALS))
endif
endif

.PHONY: init
init:
	@if [ -z "$(N)" ]; then \
		echo "用法: make init 037"; \
		exit 1; \
	fi; \
	if [ -f "$(SRC_DIR)/$(N).hs" ]; then \
		echo "错误: src/$(N).hs 已存在"; \
		exit 1; \
	fi; \
	if grep -q "^executable euler$(N)$$" euler.cabal; then \
		echo "错误: euler.cabal 中已注册 executable euler$(N)"; \
		exit 1; \
	fi; \
	cp "$(SRC_DIR)/template" "$(SRC_DIR)/$(N).hs" && \
	printf '\nexecutable euler%s\n  import:           shared\n  main-is:          %s.hs\n' "$(N)" "$(N)" >> euler.cabal && \
	echo "已创建 src/$(N).hs，并注册 executable euler$(N) 到 euler.cabal"