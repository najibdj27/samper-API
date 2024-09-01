package com.unper.samper;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import com.unper.samper.model.common.AuditorAwareImpl;

@SpringBootApplication
@EnableTransactionManagement
@EnableScheduling
@EnableJpaAuditing(auditorAwareRef = "auditorAware")
public class SamperApplication {

	@Bean
	AuditorAwareImpl auditorAware() {
		return new AuditorAwareImpl();
	}
	
	public static void main(String[] args) {
		SpringApplication.run(SamperApplication.class, args);
	}

}
