package com.unper.samper.model.common;

import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;

@SpringBootApplication
@EnableJpaAuditing
class AuditingConfiguration {

	@Bean
	AuditorAwareImpl auditorAware() {
		return new AuditorAwareImpl();
	}
}
