package com.unper.samper.util;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Component;

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;

@Component
public class EmailSender {
    @Autowired
    JavaMailSender javaMailSender;

    public void sendOtpMessage(String to, String subject, String otp) throws MessagingException {
        MimeMessage message = javaMailSender.createMimeMessage();
        String body =
                " <div style='font-family: Helvetica,Arial,sans-serif;min-width:1000px;overflow:auto;line-height:2'>" +
                        "<div style='margin:50px auto;width:70%;padding:20px 0'>"+
                        "<div style='border-bottom:1px solid #eee'>"+
                        "<a href='' style='font-size:1.4em;color: #00466a;text-decoration:none;font-weight:600'>Samper</a>"+
                        "</div>"+
                        " <p style='font-size:1.1em'>Hi,</p>"+
                        " <p>Thank you for choosing Samper. Use the following OTP to complete your Reset Password procedures. OTP is valid for 5 minutes</p>"+
                        "<h2 style='background: #00466a;margin: 0 auto;width: max-content;padding: 0 10px;color: #fff;border-radius: 4px;'>"+ otp +"</h2>" +
                        "<p style='font-size:0.9em;'>Regards,<br />Samper</p>"+
                        "<hr style='border:none;border-top:1px solid #eee' />"+
                        "<div style='float:right;padding:8px 0;color:#aaa;font-size:0.8em;line-height:1;font-weight:300'>"+
                        "<p>SAMPER</p>"+
                        "<p>Jakarta</p>"+
                        "</div>"+
                        "</div>"+
                        "</div>";
        MimeMessageHelper helper = new MimeMessageHelper(message, true);
        helper.setFrom("SAMPER");
        helper.setTo(to);
        helper.setSubject(subject);
        helper.setText(body, true);
        javaMailSender.send(message);
    }
}
