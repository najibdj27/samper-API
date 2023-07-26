package com.unper.samper;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@SpringBootApplication
@EnableTransactionManagement
public class SamperApplication {

	public static void main(String[] args) {
		SpringApplication.run(SamperApplication.class, args);
	}

}
