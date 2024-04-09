#pragma once
#include "ast.h"

std::unique_ptr<AbstractSyntaxTree> &parse(const std::string& filename);
