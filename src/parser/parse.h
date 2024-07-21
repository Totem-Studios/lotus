// Copyright 2024 Lucas Norman

#pragma once

#include "ast.h"

// this is implemented in parser.y,
// and is the function to get the parsed AST of a specific filename
std::unique_ptr<AST>& parse(const std::string& filename);
