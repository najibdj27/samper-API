package com.unper.samper.model;

import javax.persistence.*;

import com.unper.samper.model.common.Audit;

import lombok.*;

@AllArgsConstructor
@NoArgsConstructor
@Entity
@Getter
@Setter
@Builder
@Table(name = "student", schema = "profile", uniqueConstraints = {@UniqueConstraint(columnNames = "NIM")})
public class Student extends Audit {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String NIM;

    @OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY, targetEntity = User.class)
    @JoinColumn(name = "user_id")
    private User user; 

    @ManyToOne(fetch = FetchType.LAZY, targetEntity = Class.class)
    @JoinColumn(name = "class_id")
    private Class kelas;

    private Boolean isLeader;
}
