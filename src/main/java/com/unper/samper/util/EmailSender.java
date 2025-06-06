package com.unper.samper.util;

import org.apache.commons.text.StringSubstitutor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Component;

import com.unper.samper.model.EmailTemplate;

import java.util.Map;

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;

@Component
public class EmailSender {
    @Autowired
    JavaMailSender javaMailSender;

    public void sendEmailWithTemplate(String to, EmailTemplate emailTemplate, Map<String, String> params) throws MessagingException {
        String subject = StringSubstitutor.replace(emailTemplate.getSubjectTemplate(), params);
        String body = StringSubstitutor.replace(emailTemplate.getBodyTemplate(), params);

        MimeMessage message = javaMailSender.createMimeMessage();

        MimeMessageHelper helper = new MimeMessageHelper(message, true);
        helper.setFrom("Samper App <no-reply@unper.ac.id>");
        helper.setTo(to);
        helper.setSubject(subject);
        helper.setText(body, true);
        javaMailSender.send(message);
    }
}
