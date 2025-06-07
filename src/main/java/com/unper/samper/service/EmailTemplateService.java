package com.unper.samper.service;

import com.unper.samper.exception.TemplateNotFoundException;
import com.unper.samper.model.EmailTemplate;

public interface EmailTemplateService {
    EmailTemplate getByName(String name) throws TemplateNotFoundException;
}
