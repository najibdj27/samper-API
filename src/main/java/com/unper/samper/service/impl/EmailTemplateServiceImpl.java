package com.unper.samper.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.TemplateNotFoundException;
import com.unper.samper.model.EmailTemplate;
import com.unper.samper.repository.EmailTemplateRepository;
import com.unper.samper.service.EmailTemplateService;

@Service
public class EmailTemplateServiceImpl implements EmailTemplateService {

    @Autowired
    EmailTemplateRepository emailTemplateRepository;

    @Override
    public EmailTemplate getByName(String name) throws TemplateNotFoundException {
        return emailTemplateRepository.findByName(name).orElseThrow(() -> new TemplateNotFoundException("Email template with name "+ name +" not found!"));
    }
    
}
