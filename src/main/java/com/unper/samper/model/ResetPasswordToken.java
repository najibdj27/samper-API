package com.unper.samper.model;

import lombok.*;

import javax.persistence.*;

import java.util.Date;
import java.util.UUID;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
@Entity
public class ResetPasswordToken {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @Column(nullable = false)
    private String emailAddress;

    @Column(nullable = false, unique = true)
    private UUID token;

    private Date expiredDate;

    @PrePersist
    void onCreate(){
        this.token = UUID.randomUUID();
    }
}
