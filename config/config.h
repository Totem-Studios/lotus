// Copyright 2024 Pontus Henriksson & Neo Mannskär

#pragma once

enum RunningMode {
    DEBUG_MODE,
    DEFAULT_MODE,
    MINIMUM_MODE
};

#define MAX_UTILITY_RECURSION_DEPTH 100
#define MAX_UTILITY_LINKING_DEPTH 25
#define MAX_DIAGNOSTIC_MESSAGES_DISPLAYED 8
#define MAX_LOOP_ITERATIONS 1000000000

#define RUNNING_MODE DEFAULT_MODE
