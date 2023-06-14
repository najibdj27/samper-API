package com.unper.samper.model;

import javax.persistence.*;

import lombok.*;

@AllArgsConstructor
@NoArgsConstructor
@Entity
@Getter
@Setter
@Builder
@Table(name = "student", schema = "public")
public class Student {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Double NIM;

    @OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY, targetEntity = User.class)
    @JoinColumn(name = "user_id")
    private User user; 
}
