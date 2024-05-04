// Copyright 2024 Lucas Norman

#pragma once
#include "ast.h"

std::unique_ptr<AST> &parse(const std::string& filename);
