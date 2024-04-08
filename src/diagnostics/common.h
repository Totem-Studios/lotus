// Copyright 2023 Pontus Henriksson & Neo Mannskär

#pragma once
#include <string>
#include <vector>

#include "../../config/config.h"

#include "./internal/internal.h"

#include "./preprocessor.h"
#include "./lexer.h"
#include "./parser.h"
#include "./semantic_analyzer.h"

std::vector<std::string> error_messages(MAX_DIAGNOSTIC_MESSAGES_DISPLAYED);
std::vector<std::string> warning_messages(MAX_DIAGNOSTIC_MESSAGES_DISPLAYED);
std::vector<std::string> recommendation_messages(MAX_DIAGNOSTIC_MESSAGES_DISPLAYED);
