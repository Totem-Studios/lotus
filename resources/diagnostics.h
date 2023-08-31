//
// Created by neoma on 2023-08-31.
//

#ifndef LOTUS_DIAGNOSTICS_H
#define LOTUS_DIAGNOSTICS_H
#endif //LOTUS_DIAGNOSTICS_H

namespace diagnostics {
    void internal_error(const std::chrono::high_resolution_clock::time_point time, std::string header, const std::string& description, const std::string& desc_var = "", bool optional_terminator = true) {
        std::string time_spacing = std::to_string(std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now()-time).count());
        if (time_spacing.length() < 6) {
            time_spacing = std::string(6 - time_spacing.length(), ' ') + time_spacing;
        }
        std::string terminator = optional_terminator ? "! process terminated" : "";
        std::cerr << "lotus ~ internal error: " << header << "\n" << time_spacing << description << desc_var << "\n" << std::string(time_spacing.length(), ' ') << terminator << std::endl;
    }

    void preprocessor_error(const std::chrono::high_resolution_clock::time_point time, std::string header, const std::string& description, const std::string& desc_var = "", bool optional_terminator = true) {
        std::string time_spacing = std::to_string(std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now()-time).count());
        if (time_spacing.length() < 6) {
            time_spacing = std::string(6 - time_spacing.length(), ' ') + time_spacing;
        }
        std::string terminator = optional_terminator ? "! process terminated" : "";
        std::cerr << "lotus ~ preprocessor error: " << header << "\n" << time_spacing << description << desc_var << "\n" << std::string(time_spacing.length(), ' ') << terminator << std::endl;
    }

    void syntax_error(const std::chrono::high_resolution_clock::time_point time, std::string header, const std::string& description, const std::string& desc_var = "", bool optional_terminator = true) {
        std::string time_spacing = std::to_string(std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now()-time).count());
        if (time_spacing.length() < 6) {
            time_spacing = std::string(6 - time_spacing.length(), ' ') + time_spacing;
        }
        std::string terminator = optional_terminator ? "! process terminated" : "";
        std::cerr << "lotus ~ syntax error: " << header << "\n" << time_spacing << description << desc_var << "\n" << std::string(time_spacing.length(), ' ') << terminator << std::endl;
    }
}