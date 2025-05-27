package com.unper.samper.model;

import lombok.*;

import javax.persistence.*;

import com.unper.samper.model.constant.EType;

import java.util.Date;
import java.util.UUID;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
@Entity
@Table(name = "token", schema = "public")
public class Token {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String key;

    @Column(nullable = false, unique = true)
    private UUID token;

    @Enumerated(EnumType.STRING)
    private EType type;

    private Date expiredDate;

    @PrePersist
    void onCreate(){
        this.token = UUID.randomUUID();
    }
}
