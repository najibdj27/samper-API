package com.unper.samper.model;

import java.util.HashSet;
import java.util.Set;

import javax.persistence.*;

import com.unper.samper.model.common.Audit;

import lombok.*;

@AllArgsConstructor
@NoArgsConstructor
@Entity
@Getter
@Setter
@Builder
@Table(name = "student", schema = "public", uniqueConstraints = {@UniqueConstraint(columnNames = "NIM")})
public class Student extends Audit {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private Short NIM;

    @OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY, targetEntity = User.class)
    @JoinColumn(name = "user_id")
    private User user; 

    @Builder.Default
    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(name = "student_class", joinColumns = @JoinColumn(name = "student_id"), inverseJoinColumns = @JoinColumn(name = "class_id"))
    private Set<Class> kelas = new HashSet<>();
}
