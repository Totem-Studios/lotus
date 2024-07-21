// Copyright 2024 Pontus Henriksson & Neo Mannskär

#pragma once

#include <chrono>
#include <iostream>
#include <string>

namespace syntax {
static std::string
fatal_error(const std::chrono::high_resolution_clock::time_point& time,
            const std::string& header, const std::string& description,
            const std::string& desc_var = "") {
    std::string time_spacing =
        std::to_string(std::chrono::duration_cast<std::chrono::milliseconds>(
                           std::chrono::high_resolution_clock::now() - time)
                           .count());
    if (time_spacing.length() < 6) {
        time_spacing =
            std::string(6 - time_spacing.length(), ' ') + time_spacing;
    }
    std::cerr << "lotus ~ syntax error: " << header << "\n"
              << time_spacing << description << desc_var << "\n"
              << std::string(time_spacing.length(), ' ')
              << "! process terminated\n";
    exit(1);  // Later add enum error code as function parameter
}

static std::string
nonfatal_error(const std::chrono::high_resolution_clock::time_point& time,
               const std::string& header, const std::string& description,
               const std::string& desc_var = "") {
    std::string time_spacing =
        std::to_string(std::chrono::duration_cast<std::chrono::milliseconds>(
                           std::chrono::high_resolution_clock::now() - time)
                           .count());
    if (time_spacing.length() < 6) {
        time_spacing =
            std::string(6 - time_spacing.length(), ' ') + time_spacing;
    }
    std::cerr << "lotus ~ syntax error: " << header << "\n"
              << time_spacing << description << desc_var << "\n"
              << std::string(time_spacing.length(), ' ') << "\n";
    return "lotus ~ syntax error: " + header + "\n" + time_spacing +
           description + desc_var + "\n" +
           std::string(time_spacing.length(), ' ') + "\n";
}
}  // namespace syntax
